// This script is to be run in Google Earth Engine to process NDVI data and create an animation for 2020-2025.

var text = require('users/gena/packages:text')

// Fetch MODIS NDVI collection and select NDVI
var col = ee.ImageCollection('MODIS/006/MOD13A2').select('NDVI');

// Define a mask to clip the NDVI data
var mask = ee.FeatureCollection('USDOS/LSIB_SIMPLE/2017')
  .filter(ee.Filter.eq('country_na', 'United States'));

// Define the regional bounds of animation frames
var region = ee.Geometry.Polygon(
  [[[-124.72584, 49.38436],
    [-124.72584, 24.49813],
    [-66.945392, 24.49813],
    [-66.945392, 49.38436]]],
  null, false
);

// Define geometry for the legend bar
var geometryGradientBar = 
    ee.Geometry.Polygon(
        [[[-117.87578125, 28.906915730294614],
          [-117.87578125, 27.51284927518575],
          [-108.2078125, 27.51284927518575],
          [-108.2078125, 28.906915730294614]]], null, false);

// Add day-of-year (DOY) property to each image
col = col.map(function(img) {
  var doy = ee.Date(img.get('system:time_start')).getRelative('day', 'year');
  return img.set('doy', doy);
});

// Get a collection of distinct images by 'doy'
var distinctDOY = col.filterDate('2020-01-01', '2023-02-03');

// Define a filter that identifies which images from the complete
// collection match the DOY from the distinct DOY collection
var filter = ee.Filter.equals({leftField: 'doy', rightField: 'doy'});

// Define a join for the doy matches
var join = ee.Join.saveAll('doy_matches');

// Apply the join and convert the resulting FeatureCollection to an ImageCollection
var joinCol = ee.ImageCollection(join.apply(distinctDOY, col, filter));

// Apply median reduction among matching DOY collections
var comp = joinCol.map(function(img) {
  var doyCol = ee.ImageCollection.fromImages(
    img.get('doy_matches')
  );
  return doyCol.reduce(ee.Reducer.median())
    .copyProperties(img, ['system:time_start']);
});

// Define RGB visualization parameters
var visParams = {
  min: 0.0,
  max: 9000.0,
  palette: [
    'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
    '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
    '012E01', '011D01', '011301'
  ],
};

// get text locations for the dates (pt) and for the NDVI label (pt2)
var pt = text.getLocation(region, 'right', '2%', '31%')
var pt2 = text.getLocation(region, 'left', '80%', '1%')

// Add a color gradient bar with a label
geometryGradientBar = geometryGradientBar; // <-- this is a drawn geometry;
var style = require('users/gena/packages:style');
var utils = require('users/gena/packages:utils');
var text = require('users/gena/packages:text');

var min = 0;
var max = 1;
var textProperties = {
  fontSize: 32,
  textColor: 'ffffff',
  outlineColor: '000000',
  outlineWidth: 0,
  outlineOpacity: 0.6
};
var labels = ee.List.sequence(min, max);
var gradientBar = style.GradientBar.draw(geometryGradientBar, {
  min: min, max: max, palette: visParams.palette, labels: labels,
  format: '%.0f', text: textProperties
});

// NDVI label and legend
var leglabel = 'NDVI';
var legscale = Map.getScale() * 1; // scale text font relative to the current map scale
var legtext = text.draw(leglabel, pt2, legscale, {fontSize: 32});

// Create RGB visualization images for use as animation frames
var rgbVis = comp.map(function(img) {
  var scale = 10000
  var textVis = { fontSize: 32, textColor: 'ffffff', outlineColor: '000000', outlineWidth: 2.5, outlineOpacity: 0.6 }
  var label = text.draw(img.get('system:index'), pt, scale, textVis)
  
  return img.visualize(visParams).clip(mask).blend(label).blend(gradientBar).blend(legtext)
});

// Define GIF visualization arguments
var gifParams = {
  'region': region,
  'dimensions': 600,
  'crs': 'EPSG:3857',
  'framesPerSecond': 10,
  'format': 'gif'
};

// Print the GIF URL to the console
print(rgbVis.getVideoThumbURL(gifParams));

// Render the GIF animation in the console
print(ui.Thumbnail(rgbVis, gifParams));

// Export animated video to Google Drive
Export.video.toDrive({
  collection: rgbVis,       // ImageCollection to export
  description: 'NDVI_Animation_2020-2025',  // Name
  folder: 'NDVI_Data',  // Google Drive folder
  fileNamePrefix: 'NDVI_animation', // Name of the output file
  framesPerSecond: 10,       // Animation speed
  dimensions: 1800,           // Output resolution
  region: region,            // Clip to region
  maxPixels: 1e9             // Prevents export failure on large datasets
});