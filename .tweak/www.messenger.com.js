function removeAppAd(elem) {
  const ad = elem.querySelector("a[aria-label*='for Mac']");
  if (ad == null) {
    console.log("ad not found");
    return;
  }

  console.log("removing ad");
  ad.parentElement.parentElement.remove();
}

// Find the side bar when it loads and then remove the messenger app for mac ad
function initWhenContentReady() {
  const sidebar = document.querySelector("div[role=navigation]");
  if (sidebar == null) {
    window.setTimeout(initWhenContentReady, 1000);
  } else {
    removeAppAd(sidebar);
  }
}

initWhenContentReady();
