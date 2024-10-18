
async function getImageBlobFromUrl(url) {
  const fetchedImageData = await fetch(url);
  const blob = await fetchedImageData.blob();
  return blob;
}
$(document).ready(function () {
  $("#stat_n_focus_line-copybtn").on("click", async () => {
    const src = $("#stat_n_focus_line-copy_plot>img").attr("src");
    try {
      const blob = await getImageBlobFromUrl(src);
      await navigator.clipboard.write([
        new ClipboardItem({
          [blob.type]: blob
        })
      ]);
      alert("Image copied to clipboard!");
    } catch (err) {
      console.error(err.name, err.message);
      alert("There was an error while copying image to clipboard :/");
    }
  });
});
