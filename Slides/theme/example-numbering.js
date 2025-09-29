// Automatically number example blocks in HTML output
(function() {
  document.addEventListener('DOMContentLoaded', function() {
    var examples = document.querySelectorAll('.example-block');
    examples.forEach(function(el, i) {
      var heading = el.querySelector('h1, h2, h3, h4, h5, h6');
      if (heading) {
        heading.textContent = `Example ${i+1}: ` + heading.textContent.replace(/^Example\s*\d*:?\s*/, '');
      }
    });
  });
})();
