async function getImageBlobFromUrl(url) {
  const fetchedImageData = await fetch(url);
  const blob = await fetchedImageData.blob();
  return blob;
}

function setupCopyButton(buttonId, imgSelector) {
  $(document).ready(function () {
    $(buttonId).on("click", async () => {
      const src = $(imgSelector).attr("src");
      if (!src) {
        alert("No image found to copy.");
        return;
      }

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
        alert("There was an error while copying the image to clipboard :/");
      }
    });
  });
}

// Set up the copy buttons for different image sources
setupCopyButton("#copybtn_focus_line", "#stat_n_focus_line-copy_plot>img");
setupCopyButton("#copybtn_multi_line", "#stat_n_multi_line-copy_plot>img");

