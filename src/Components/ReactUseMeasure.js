const React = require("react")
const useMeasure = require("react-use-measure")

exports.useMeasure_ = props => {
  const [ref, bounds] = useMeasure(props)
  return props.render(ref, bounds)
}
