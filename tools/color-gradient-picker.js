var gradient = JSON.parse (process.argv.slice (-2, -1) [0]);
var value = parseFloat(process.argv.slice (-1) [0], 10);
function componentToHex(c) {
  var hex = c.toString(16);
  return hex.length == 1 ? "0" + hex : hex;
}
function hexToRgb(hex) {
  // Expand shorthand form (e.g. "03F") to full form (e.g. "0033FF")
  var shorthandRegex = /^#?([a-f\d])([a-f\d])([a-f\d])$/i;
  hex = hex.replace(shorthandRegex, function(m, r, g, b) {
    return r + r + g + g + b + b;
  });

  var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
  return result ? result : null;
}
function rgbToHex(r, g, b) {
  return "#" + componentToHex(r) + componentToHex(g) + componentToHex(b);
}
function pickHex(color1, color2, weight) {
    var p = weight;
    var w = p * 2 - 1;
    var w1 = (w/1+1) / 2;
    var w2 = 1 - w1;
    return rgbToHex (Math.round(color1[0] * w1 + color2[0] * w2),
        Math.round(color1[1] * w1 + color2[1] * w2),
        Math.round(color1[2] * w1 + color2[2] * w2));
}
var colorRange = [];
gradient.forEach (function (v, index) {
  if(value<=v[0] && colorRange.length === 0) {
      colorRange = [index-1,index]
  }
});
//Get the two closest colors
var firstcolor = gradient[colorRange[0]][1];
var secondcolor = gradient[colorRange[1]][1];
firstcolor = typeof firstcolor === 'string' ? hexToRgb (firstcolor) : firstcolor;
secondcolor = typeof secondcolor === 'string' ? hexToRgb (secondcolor) : secondcolor;

//Calculate ratio between the two closest colors
var firstcolor_x = (gradient[colorRange[0]][0]/100);
var secondcolor_x = (gradient[colorRange[1]][0]/100)-firstcolor_x;
var slider_x = (value/100)-firstcolor_x;
var ratio = slider_x/secondcolor_x
        
//Get the color with pickHex(thx, less.js's mix function!)
var result = pickHex( secondcolor,firstcolor, ratio );
console.log (result)
