"use strict"

const postcss = require('postcss');
const fs = require('fs');

function addImportant(root, result) {
  root.walkDecls(decl => {
    decl.important = true;
  });

}

fs.readFile(process.argv[2], (err, css) => {
  postcss()
  .use(addImportant)
  .process(css)
  .then(result => {
    console.log(result.css);
  })
});
