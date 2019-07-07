const path = require ('path');
module.exports = {
    mode: "development",
    entry: "./main.js",
    output: {filename: "safeguard_ts.js", path: __dirname},
    target: "node"
};
