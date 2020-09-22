
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


// cvim like bindings
map('J', 'E')
map('K', 'R')
map('H', 'S')
map('L', 'D')
map('i', '<Alt-i>')
map('F', 'gf')

unmapAllExcept(['i', 'f', 'J', 'K', '<Esc>'], /messenger.com/)
mapkey('<Ctrl-j>', 'next convo', function () {
    var sibling = document
        .querySelector('[aria-label="Conversation List"] > [aria-relevant]')
        .nextSibling;

    if (sibling !== null) {
        sibling
            .querySelector('a')
            .click()
    }


}, {domain: /messenger.com/})


mapkey('<Ctrl-k>', 'next convo', function () {
    var sibling = document
        .querySelector('[aria-label="Conversation List"] > [aria-relevant]')
        .previousElementSibling;

    if (sibling !== null) {
        sibling
            .querySelector('a')
            .click()
    }

}, {domain: /messenger.com/})

if ( document.origin === "https://www.messenger.com" ) {
    // don't focus input box
    console.log('hello')
    settings.stealFocusOnLoad = true;
}



unmapAllExcept(['i', 'f', 'J', 'K', '<Esc>'], /mail.google.com/)


Hints.style('font-family:system-ui;font-weight: normal;');

iunmap(":");
