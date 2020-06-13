const React = require("react")
const Spring = require("react-spring/renderprops").Spring

exports.spring_ = props => React.createElement(
  Spring,
  Object.assign({}, props, { children: props.render })
)
