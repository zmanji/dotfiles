var focus = function() {
    var current = document
        .querySelector('[contenteditable="true"][aria-label="Type a message..."]')
    if (current != null) {
      current.focus();
    }
}

focus();
