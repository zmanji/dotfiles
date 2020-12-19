var focus = function() {
    var current = document
        .querySelector('[contenteditable="true"][role=textbox]')
    if (current != null) {
      current.focus();
    }
}

focus();
