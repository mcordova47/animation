const React = require("react")

const ReactMotion = require("react-motion")

exports.spring_ = ReactMotion.spring
exports.motion_ = props => React.createElement(
  ReactMotion.Motion,
  Object.assign({}, props, { children: props.render })
)
