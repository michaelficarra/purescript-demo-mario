// module Mario.DOM

exports.onDOMContentLoaded =
  function onDOMContentLoaded(action) {
    return function() {
      if (document.readyState === "interactive") {
        action();
      } else {
        document.addEventListener("DOMContentLoaded", action);
      }
      return {};
    };
  };

exports.getMarioNode =
  function getMarioNode() {
    return document.getElementById("mario");
  };
