var LandsatLST = require('users/sofiaermida/landsat_smw_lst:modules/Landsat_LST.js')


var geometry = ee.Geometry.Polygon(
    [[[6.463619346963969, 46.613125326260246],
    [6.463619346963969, 46.46836511510258],
    [6.820331688272563, 46.46836511510258],
    [6.820331688272563, 46.613125326260246]]], null, false);

var setBuffer = false;
// in meters
var bufferDistance = 15;

var exportTiffMed = true;
var exportTiffMean = false;

var fileName = 'lstGAF.csv'

// Define a function to create an ee.Point from a feature
var createPoint = function (feature) {
    var x = feature.get("X");
    var y = feature.get("Y");
    var pt = feature.get("pt");
    var gaf = feature.get("F2_GAF_current");
    var gaf_adj = feature.get("F2_GAF_current_adj");
    var val = ee.Feature(ee.Geometry.Point([x, y]));
    if (setBuffer) {
        val = val.buffer({ 'distance': bufferDistance, 'units': 'meters' });
    }
    val = val.set("X", x);
    val = val.set("Y", y);
    val = val.set("pt", pt);
    val = val.set("GAF", gaf);
    val = val.set("GAF_ADJ", gaf_adj)
    return val
};

// Use the map function to apply the createPoint function to each feature in the collection
var points = table.map(createPoint);
print(points)

var satellite = 'L5';
var date_start = '-06-01';
var date_end = '-08-31';
var use_ndvi = true;

var years = [1998, 2008, 2018]
var finalCollection;

var selectedCols = ['pt', 'X', 'Y', 'GAF', 'GAF_ADJ']

var pointsFC = ee.FeatureCollection(points.map(function (point) {
    return ee.Feature(point);
}));

for (var i = 0; i < years.length; i++) {

    var year = years[i]

    if (year > 2012) {
        satellite = 'L8'
    }

    if (year == 2012) {
        satellite = 'L7'
    }
    var LandsatColl = LandsatLST.collection(satellite, year + date_start, year + date_end, geometry, use_ndvi);
    print("For:", year, "Number of images used: ", LandsatColl.size());
    var exImageMean = LandsatColl.mean();
    var exImageMedian = LandsatColl.median();
    var lstImgMean = ee.Image(exImageMean.select(['LST', 'NDVI']));
    var lstImgMedian = ee.Image(exImageMedian.select(['LST', 'NDVI']));
    lstImgMean = lstImgMean.select(['LST', 'NDVI']).rename(['lstMean' + year, 'NDVIMean' + year])
    lstImgMedian = lstImgMedian.select(['LST', 'NDVI']).rename(['lstMedian' + year, 'NDVIMedian' + year])

    var MeanValuesArray = lstImgMean.reduceRegions({
        collection: pointsFC,  // Use the pointsFC feature collection as the region over which to extract values
        reducer: ee.Reducer.mean(),  // Use the mean reducer to calculate the mean value of each band
        scale: 30,  // Set the scale to 30 meters/pixel
    });


    var MedValuesArray = lstImgMedian.reduceRegions({
        collection: pointsFC,  // Use the pointsFC feature collection as the region over which to extract values
        reducer: ee.Reducer.median(),  // Use the median reducer to calculate the mean value of each band
        scale: 30,  // Set the scale to 30 meters/pixel
    });

    if (exportTiffMean) {
        Export.image.toDrive({
            image: lstImgMean,
            description: 'lstMean' + year,
            scale: 30,
            region: geometry,
        });
    }

    if (exportTiffMed) {
        Export.image.toDrive({
            image: lstImgMedian,
            description: 'lstMedian' + year,
            scale: 30,
            region: geometry,
        });
    }

    var tmpColl = MeanValuesArray.merge(MedValuesArray)

    if (finalCollection === undefined) {
        finalCollection = tmpColl
    } else {
        finalCollection = finalCollection.merge(tmpColl)
    }
    selectedCols.push('lstMean' + year, 'NDVIMean' + year, 'lstMedian' + year, 'NDVIMedian' + year)
}
print(selectedCols)
// We can't print more then 5k rows
print(finalCollection.limit(10))

// Export the FeatureCollection to a csv file
Export.table.toDrive({
    collection: finalCollection,
    description: fileName,
    fileFormat: 'CSV',
    selectors: selectedCols
})

// WARNING: The extracted CSV needs to be preprocessed to be merged in a well defined order.