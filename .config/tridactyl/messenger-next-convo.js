var godown = function () {
    var current = document
        .querySelector('[aria-label="Conversation List"] > [aria-relevant]:not([aria-live="polite"])')

    var sibling = current.nextSibling;

    if (sibling !== null) {
        var link = sibling.querySelector('[role=link]');
        if (link !== null) {
          link.click();
        }
    }
};

godown();

