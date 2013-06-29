{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Data.D3Plot (
    plotFull,
    Plottable (plot, plots, plotWithOpts),
    PlotOptions(PlotOptions, plotPath, plotAutoRefresh, plotFileName),     
    Color,
    defaultOpts
    ) where

import Text.Hamlet
import Text.Julius
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html (Markup, ToMarkup)

import System.IO
import System.FilePath.Posix
import System.Directory

import Paths_D3Plot

-- | Options that specify the plotting behaviour
data PlotOptions = PlotOptions {
    -- | Path where to place the resulting HTML file and it's includes. 
    --   Default is current directory
    plotPath        :: String,

    -- | Set this to true to have the resulting HTML file automatically refresh
    --   every few seconds using javascript. Default is false.
    plotAutoRefresh :: Bool,

    -- | The name of the resulting HTML file. Default is \"plot.html\"
    plotFileName    :: String
}

defaultOpts :: PlotOptions
defaultOpts  = PlotOptions "." False "plot.html"

-- | Anything that's a valid HTML color can go here.
type Color = String

-- | Convenience class for quickly plotting data. Create instances for whatever
--   data you are using. You only need to implement plotWithOpts and the other 
--   methods should just work.
class Plottable a where 
    -- | The default implementation just uses the default options.
    plot         :: a -> IO ()
    plot = plotWithOpts defaultOpts . return

    -- | The default implementation just uses the default options.
    --   Use this methods if plotting multiple objects at the same time.
    plots :: [a] -> IO ()
    plots = plotWithOpts defaultOpts

    plotWithOpts :: PlotOptions -> [a] -> IO ()

instance (ToMarkup a, Num a, ToMarkup b, Num b) => Plottable [(a, b)] where
    plotWithOpts opts values = plotFull opts labeled 
        where
            labeled = map (makePlot (length values)) . zip [0..] $ values

instance (ToMarkup a, Num a) => Plottable [a] where
    plotWithOpts opts values = plotFull opts labeled 
        where 
            labeled  = map (makePlot (length values)) . zip [0..] $ numbered
            numbered = map (zip [(0 :: Int)..]) values

makePlot :: Int -> (Int, [a]) -> (String, Color, [a])
makePlot total (n, values) =  ((show n), (generateColor total n), values)

-- | This method does all the work. Each item in the supplied list will be
--   plotted as a separate line. First element of the tuple is the key, second
--   is the colour and the last are a list of X and Y coordinates of each point.
plotFull :: (ToMarkup a, Num a, ToMarkup b, Num b) => PlotOptions -> [(String, Color, [(a, b)])] -> IO ()
plotFull opts values = writeResult opts $ renderHtml template
    where 
        template = [shamlet|
            <html>
                #{htmlHead}

                <body>
                    <div id="chart"><svg>

                    <script>
                        #{chart "chart_data"}      
                        #{buildData "chart_data" values} 

                    #{autoRefresh (plotAutoRefresh opts)}        
        |]

htmlHead :: Markup
htmlHead = [shamlet| 
    <script src="http://d3js.org/d3.v3.min.js" charset="utf-8">
    <script src="nv.d3.min.js" charset="utf-8">
    <link href="nv.d3.css" rel="stylesheet" type="text/css">
|]

chart :: String -> Markup
chart dataFunction = [shamlet| 
    nv.addGraph(function() {  
        var chart = nv.models.lineChart();
   
        d3.select('#chart svg')
            .datum(#{dataFunction ++ "()"})
            .transition().duration(200)
            .call(chart);

        nv.utils.windowResize(function() { d3.select('#chart svg').call(chart) });

        return chart;
    });
|]


buildData :: (ToMarkup a, Num a, ToMarkup b, Num b) => String -> [(String, Color, [(a, b)])] -> Markup 
buildData dataFunction plots = [shamlet|
        function #{dataFunction}() {
            var result = [];
            $forall (label, color, values) <- plots
                result.push({
                    key: '#{label}',
                  color: '#{color}',
                 values: #{buildDataValues values}
                });
            return result;
        }
    |]

buildDataValues :: (ToMarkup a, Num a, ToMarkup b, Num b) => [(a, b)] -> Markup
buildDataValues values = [shamlet| 
        [
        $forall (x, y) <- values
            { x : #{x}, y : #{y}},
        ]    
    |]

autoRefresh :: Bool -> Markup
autoRefresh enabled = if enabled 
                        then [shamlet| <script> setInterval(location.reload, 1000); |]
                        else [shamlet| |]

generateColor :: Int -> Int -> String
generateColor total number = "hsl(" ++ show hue ++ ",20%, 50%)"
    where step = 360 / (fromIntegral total)
          hue  = (fromIntegral number) * step

writeResult :: PlotOptions -> String -> IO ()
writeResult opts html = do
    moveResourceFiles $ plotPath opts
    writeFile (plotPath opts </> plotFileName opts) html

moveResourceFiles :: FilePath -> IO ()    
moveResourceFiles path = do
    nvJS  <- getDataFileName "resources/nv.d3.min.js"
    nvCSS <- getDataFileName "resources/nv.d3.css"

    createDirectoryIfMissing True path

    copyFile nvJS  $ path </> (takeFileName nvJS)
    copyFile nvCSS $ path </> (takeFileName nvCSS)

    