var godown = function () {
    var links = Array.from(document.querySelectorAll('[role=navigation] a[role=link][href^="/t"]'));

    var current = links.filter(l => {
      var attrs = l.attributes || new NamedNodeMap();
      var label = attrs.getNamedItem("aria-label") || {};
      if (label.value && label.value.startsWith('Chats')) {
        return false;
      }
      var u = new URL(l.href)
      return u.href === window.location.href || u.href.startsWith(window.location.href) || window.location.href.startsWith(u.href);
    })


  console.log(current);
  if (current.length > 0) {
    current = current[0]
  } else {
    return;
  }

   var afterCurrent = false;

   if (current !== null) {
     for (const l of links) {
       if (afterCurrent === true) {
         l.click();
         return;
       }

       if (l === current) {
         afterCurrent = true;
       }
     }
   }
};

godown();

