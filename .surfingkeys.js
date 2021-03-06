
settings.theme = `
    #sk_status, #sk_find {
        font-family:system-ui;
        right: 5%;
    }
}`;
settings.showModeStatus = true
settings.smoothScroll = false

var navSites = /gmail.com|twitter.com|feedly.com|reddit.com/

unmap('j', navSites);
unmap('k', navSites);

unmap('l', /twitter.com/)

unmap('r', /feedly.com/)


// cvim like bindings
map('J', 'E')
map('K', 'R')
map('H', 'S')
map('L', 'D')
map('i', '<Alt-i>')
map('F', 'C')


unmapAllExcept(['J', 'K', '<Esc>'], /todoist.com/)

unmapAllExcept(['i', 'f', 'J', 'K', '<Esc>'], /messenger.com/)

var godown = function () {
    var sibling = document
        .querySelector('[aria-label="Conversation List"] > [aria-relevant]')
        .nextSibling;

    if (sibling !== null) {
        var link = sibling.querySelector('[role=link]');
        if (link !== null) {
          link.click();
        }
    }


};


mapkey('<Ctrl-j>', 'next convo', godown, {domain: /messenger\.com/i});
imapkey('<Ctrl-j>', 'next convo', godown, {domain: /messenger\.com/i});

var goup = function () {
    var sibling = document
        .querySelector('[aria-label="Conversation List"] > [aria-relevant]')
        .previousElementSibling;

    if (sibling !== null) {
        var link = sibling.querySelector('[role=link]');
        if (link !== null) {
          link.click();
        }
    }

};

mapkey('<Ctrl-k>', 'next convo', goup, {domain: /messenger\.com/i});
imapkey('<Ctrl-k>', 'next convo', goup, {domain: /messenger\.com/i});

if (self.origin === "https://www.messenger.com" ) {
    // don't focus input box
    settings.enableAutoFocus = true;
}



unmapAllExcept(['i', 'f', 'J', 'K', '<Esc>'], /mail.google.com/)


Hints.style('font-family:system-ui;font-weight: normal;');

iunmap(":");
