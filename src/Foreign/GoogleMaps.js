exports.loadScriptImpl = require("@react-google-maps/api").LoadScript;
exports.googleMapImpl = require("@react-google-maps/api").GoogleMap;
exports.markerImpl = require("@react-google-maps/api").Marker;
exports.infoWindowImpl = require("@react-google-maps/api").InfoWindow;

exports.getMapBoundsPromise = function () {
  console.log("In promise");
  const map = require("@react-google-maps/api").useGoogleMap();
  console.log("got map");
  console.log(map.getBounds());
  console.log("got bounds");
  React.useEffect(() => {
    if (map) {
      console.log(map.getBounds());
    }
  }, [map]);
};
