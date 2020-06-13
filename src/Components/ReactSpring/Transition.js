const React = require("react")
const Transition = require("react-spring/renderprops").Transition

exports.transition_ = props => React.createElement(
  Transition,
  Object.assign({}, props, { children: props.render })
)
