const React = require("react")

exports.attachRef = ref => node => React.cloneElement(node, { ref })
