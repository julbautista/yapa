var w=10000,h=7000,
svg=d3.select("#chart")
.append("svg")
.attr("width",w)
.attr("height",h);

var text=svg
.append("text")
.text("hello world")
.attr("y",50);

alert("hello!");
