{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Data.D3Plot.DiGraph where


import Text.Hamlet
import Text.Julius
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html (Markup, ToMarkup, preEscapedToHtml)

import System.IO
import System.FilePath.Posix
import System.Directory

import Data.String.Utils

import Control.Arrow ((***))

class GraphNode a where
    nodeName :: a -> String

instance GraphNode String where
    nodeName = id    

diGraphMarkup :: GraphNode a => (Int,Int) -> String -> [a] -> [(Int, Int)] -> Markup
diGraphMarkup (w,h) container nodes edges = [shamlet|
        var svg = d3.select("##{container}").append("svg");
        
        #{nodeData nodes}
        
        #{edgeData edges}

        var force = d3.layout.force()
                         .nodes(nodeData)
                         .links(edgeData)
                         .size([#{w}, #{h}])
                         .start();

        var nodes = svg.selectAll("circle")
            .data(nodeData)
            .enter()
            .append("circle")
            .attr("r", 10)
            .style("fill", function(d, i) {
                    return "#FF0000";
            })
            .call(force.drag);

        var edges = svg.selectAll("line")
            .data(edgeData)
            .enter()
            .append("line")
            .style("stroke", "#ccc")
            .style("stroke-width", 1);

        force.on("tick", function() {

            edges.attr("x1", function(d) { return d.source.x; })
                 .attr("y1", function(d) { return d.source.y; })
                 .attr("x2", function(d) { return d.target.x; })
                 .attr("y2", function(d) { return d.target.y; });

            nodes.attr("cx", function(d) { return d.x; })
                 .attr("cy", function(d) { return d.y; });

        });
    
    |]


multiLookup :: Eq a => a -> [(a,b)] -> [b]
multiLookup key = map snd . filter ((key==) . fst)

nodeData :: GraphNode a => [a] -> Markup
nodeData nodes = [shamlet|
        var nodeData = [
        $forall node <- nodes
            { name : "#{nodeName node}"},
        ];        
    |]

edgeData :: [(Int, Int)] -> Markup 
edgeData edges = [shamlet| 
        var edgeData = [
        $forall (from, to) <- edges
            {source : #{from}, target: #{to} },
        ];
    |]
