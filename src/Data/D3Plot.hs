{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Data.D3Plot (
    plotLineChart,
    plotBarChart,
    LinePlottable (linePlot, linePlots, linePlotWithOpts),
    BarPlottable (barPlot, barPlots, barPlotWithOpts),
    PlotOptions(PlotOptions, plotPath, plotAutoRefresh, plotFileName),     
    Color,
    defaultOpts,
    generateColor
    ) where

import Text.Hamlet
import Text.Julius
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html (Markup, ToMarkup, preEscapedToHtml)

import System.IO
import System.FilePath.Posix
import System.Directory

import Control.Arrow ((***))

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
--   data you are using. You only need to implement plotLineChart and the other 
--   methods should just work.
class LinePlottable a where 
    -- | The default implementation just uses the default options.
    linePlot         :: a -> IO ()
    linePlot = linePlotWithOpts defaultOpts . return

    -- | The default implementation just uses the default options.
    --   Use this methods if plotting multiple objects at the same time.
    linePlots :: [a] -> IO ()
    linePlots = linePlotWithOpts defaultOpts

    linePlotWithOpts :: PlotOptions -> [a] -> IO ()

instance (ToMarkup a, Num a, ToMarkup b, Num b) => LinePlottable [(a, b)] where
    linePlotWithOpts opts values = plotLineChart opts labeled 
        where
            labeled = map (makePlot (length values)) . zip [0..] $ values

instance (ToMarkup a, Num a) => LinePlottable [a] where
    linePlotWithOpts opts values = plotLineChart opts labeled 
        where 
            labeled  = map (makePlot (length values)) . zip [0..] $ numbered
            numbered = map (zip [(0 :: Int)..]) values

-- | Same as LinePlottable but it wraps plotBarChart and plots a bar chart
--   instead of a line chart.
class BarPlottable a where
    barPlot  ::  a  -> IO ()
    barPlot = barPlotWithOpts defaultOpts . return 

    barPlots :: [a] -> IO ()
    barPlots = barPlotWithOpts defaultOpts

    -- | When implementing this class you only need to to provide this function
    --   and the other two a will have default implimentations.
    barPlotWithOpts :: PlotOptions -> [a] -> IO ()

instance (Show a, ToMarkup b, Num b) => BarPlottable [(a, b)] where
    barPlotWithOpts opts values = plotBarChart opts labeled 
        where
            textValues = map (map (show *** id)) values
            labeled    = zip3 (repeat "") (colors size) textValues
            size       = length values

makePlot :: Int -> (Int, [a]) -> (String, Color, [a])
makePlot total (n, values) =  ((show n), (generateColor total n), values)

-- | This method plots a line graph. Each item in the supplied list will be
--   plotted as a separate line. First element of the tuple is the key, second
--   is the colour and the last are a list of X and Y coordinates of each point.
plotLineChart :: (ToMarkup a, ToMarkup b, Num b) 
         => PlotOptions
         -> [(String, Color, [(a, b)])] 
         -> IO ()
plotLineChart = render lineChart (buildData xyNumberValues)


-- | This method plots a bar chart. Each item in the supplied list will be
--   plotted as a separate bar chart (a collection of bars of the same colour).
--   First element of the tuple is the key, second is the colour and the last 
--   are a list of X labels and Y coordinates of each bar in this bar chart.
plotBarChart :: (ToMarkup b, Num b) 
         => PlotOptions
         -> [(String, Color, [(String, b)])] 
         -> IO ()
plotBarChart = render barChart (buildData textLabelValues)       


render :: (ToMarkup a, ToMarkup b, Num b) 
       => (String  -> Markup) 
       -> (String -> [(String, Color,[(a,b)])] -> Markup)
       -> PlotOptions 
       -> [(String, Color, [(a, b)])] 
       -> IO ()     
render plotter dataBuilder opts values = writeResult opts $ renderHtml template
    where 
        template = [shamlet|
            <html>
                #{htmlHead}

                <body>
                    <div id="chart"><svg>

                    <script>
                        #{plotter "chart_data"}      
                        #{dataBuilder "chart_data" values} 

                    #{autoRefresh (plotAutoRefresh opts)}        
        |]


htmlHead :: Markup
htmlHead = [shamlet| 
    <script src="http://d3js.org/d3.v3.min.js" charset="utf-8">
    <script src="nv.d3.min.js" charset="utf-8">
    <link href="nv.d3.css" rel="stylesheet" type="text/css">
|]

barChart :: String -> Markup
barChart dataFunction = [shamlet| 
     nv.addGraph(function() {  
        var chart = nv.models.discreteBarChart();
        
        var data  = #{dataFunction}();

        var getMax = function(d){
            return d3.max(d.values, function(v){ return v.y} );
        }

        chart.yDomain([0, d3.max(data, getMax)]);

        d3.select('#chart svg')
            .datum(data)
            .transition().duration(200)
            .call(chart);

        nv.utils.windowResize(function() { d3.select('#chart svg').call(chart) });

        return chart;
    });

|]

lineChart :: String -> Markup
lineChart dataFunction = [shamlet| 
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

buildData :: (ToMarkup a, ToMarkup b, Num b) 
          => ([(a, b)] -> Markup)
          -> String
          -> [(String, Color, [(a, b)])] -> Markup 
buildData convertValues dataFunction plots = [shamlet|
        function #{dataFunction}() {
            var result = [];
            $forall (label, color, values) <- plots
                result.push({
                    key: '#{label}',
                  color: '#{color}',
                 values:  #{convertValues values}
                });
            return result;
        }
    |]

xyNumberValues :: (ToMarkup a, ToMarkup b, Num b) => [(a, b)] -> Markup
xyNumberValues values = [shamlet| 
        [
        $forall (x, y) <- values
            { x : #{preEscapedToHtml x}, y : #{y}},
        ]    
    |]

textLabelValues :: (ToMarkup a, Num a) => [(String, a)] -> Markup
textLabelValues values = [shamlet| 
        [
        $forall (x, y) <- values
            { x : "#{x}", y : #{y}},
        ]    
    |]


autoRefresh :: Bool -> Markup
autoRefresh enabled = if enabled 
                        then [shamlet| <script> setInterval(location.reload, 1000); |]
                        else [shamlet| |]

-- | Helper to generate colours for a plot. The first argument is the number of 
--   of graphs and the second is the index of this graph. Returns a unique 
--   colour for every plot maximally spaced out on the colour wheel.     
generateColor :: Int -> Int -> Color
generateColor total number = "hsl(" ++ show hue ++ ",20%, 50%)"
    where step = 360 / (fromIntegral total)
          hue  = (fromIntegral number) * step

colors :: Int -> [Color]          
colors total = map (generateColor total) [0..total]

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

    