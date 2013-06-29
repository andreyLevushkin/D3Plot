{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Data.D3Plot (plotFull, Plottable (plot, plotWithOpts), PlotOptions(PlotOptions), Color) where

import Text.Hamlet
import Text.Julius
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html (Markup)

import System.IO
import System.FilePath.Posix
import System.Directory

import Paths_D3Plot

data PlotOptions = PlotOptions {
    plotPath        :: String,
    plotAutoRefresh :: Bool,
    plotFileName    :: String
}

defaultOpts :: PlotOptions
defaultOpts  = PlotOptions "." False "plot.html"

type Color = String

class Plottable a where 
    plot         :: a -> IO ()
    plot = plotWithOpts defaultOpts

    plotWithOpts :: PlotOptions -> a -> IO ()

instance (Show a, Num a) => Plottable [a] where
    plotWithOpts opts values = plotFull opts [("", "#000000", numbered)]
        where numbered = zip [0..] values

instance  (Show a, Num a) => Plottable [[a]] where
    plotWithOpts opts dataSets = plotFull opts . snd . foldr expand (0,[]) $ dataSets
        where
         count = length dataSets
         expand values (n, result)  = (n + 1,  expanded : result) 
            where expanded = (show n, generateColor count n, (zip [0..] values))


plotFull :: (Show a, Num a, Show b, Num b) => PlotOptions -> [(String, Color, [(a, b)])] -> IO ()
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


buildData :: (Show a, Num a, Show b, Num b) => String -> [(String, Color, [(a, b)])] -> Markup 
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

buildDataValues :: (Show a, Show b) => [(a, b)] -> Markup
buildDataValues values = [shamlet| 
        [
        $forall (x, y) <- values
            { x : #{show x}, y : #{show y}},
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

    