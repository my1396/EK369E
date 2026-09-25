// Automatically number example blocks in HTML output
// Use example:
//    In the beginning of your slide, write "class: example-block",
//  then write your example with a heading like "## Example: Title",
//  and this script will number them sequentially.
//
// To continue one example across several slides, give the follow-on slides
// "class: example-block, example-cont". They keep the number of the example
// they continue and get "(cont.)" appended, so splitting a long example over
// two slides does not consume a second example number.

(function() {
  document.addEventListener('DOMContentLoaded', function() {
    // Scope to the deck itself: xaringanExtra's tile view clones every slide.
    var root = document.querySelector('.remark-slides-area') || document;
    var examples = root.querySelectorAll('.example-block');
    var n = 0;
    examples.forEach(function(el) {
      var heading = el.querySelector('h1, h2, h3, h4, h5, h6');
      if (!heading) return;
      var cont = el.classList.contains('example-cont');
      if (!cont) n += 1;
      var title = heading.textContent
        .replace(/^Example\s*\d*:?\s*/, '')
        .replace(/\s*\(cont\.\)\s*$/, '');
      heading.textContent = 'Example ' + n + ': ' + title + (cont ? ' (cont.)' : '');
    });
  });
})();
