# D3Plot

This is a simple library to generate HTML charts using the D3 and NVD3 libraries.  The main method is: 

    plotFull :: (Show a, Num a, Show b, Num b) => PlotOptions -> [(String, Color, [(a, b)])] -> IO ()

Each element of the supplied list is plotted as a separate line on a plot with the specified name and colour.
There is also the Plottable class for quickly plotting data using the plot function for example 

    plot [1..10]

or

    plot [(1,2), (2, 20), (3, 23)]    

