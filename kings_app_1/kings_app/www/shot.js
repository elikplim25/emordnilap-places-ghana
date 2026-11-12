/* Download any block (.shot) as a PNG, ready for social media.
   Uses html-to-image (loaded from jsDelivr in app.R). While capturing, the block gets the class
   "capturing": download buttons and controls (.no-shot) are hidden and the source line (.shot-foot) appears. */
function kingsShot(btn) {
  var node = btn.closest('.shot');
  if (!node || typeof htmlToImage === 'undefined') { alert('The image tool did not load. Check your internet connection and reload.'); return; }
  var name = (node.getAttribute('data-name') || 'kings-of-israel').replace(/[^a-z0-9-]+/gi, '-').replace(/^-+|-+$/g, '').toLowerCase();
  var label = btn.innerHTML;
  btn.innerHTML = 'Saving...';
  node.classList.add('capturing');
  var bg = getComputedStyle(node).backgroundColor;
  if (!bg || bg === 'rgba(0, 0, 0, 0)' || bg === 'transparent') bg = '#FFFFFF';
  // wait two frames so the layout reflects the "capturing" class before the snapshot
  requestAnimationFrame(function () { requestAnimationFrame(function () {
    htmlToImage.toPng(node, { pixelRatio: 3, backgroundColor: bg, cacheBust: true })
      .then(function (url) {
        var a = document.createElement('a');
        a.download = name + '.png'; a.href = url;
        document.body.appendChild(a); a.click(); a.remove();
      })
      .catch(function (e) { console.error(e); alert('Sorry, the image could not be saved: ' + (e && e.message ? e.message : e)); })
      .then(function () { node.classList.remove('capturing'); btn.innerHTML = label; });
  }); });
}
