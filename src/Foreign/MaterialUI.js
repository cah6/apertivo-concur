
exports.timePickerImpl = require("@material-ui/pickers").KeyboardTimePicker
exports.pickerProviderImpl = require("@material-ui/pickers").MuiPickersUtilsProvider
exports.dateFnsUtilsImpl = require("@date-io/date-fns").default
exports.classTableContainer = require("@material-ui/core/TableContainer").default
exports.classBox = require("@material-ui/core/Box").default
exports.createMuiTheme = function(t) {
  return require("@material-ui/core/styles").createMuiTheme(t);
};
