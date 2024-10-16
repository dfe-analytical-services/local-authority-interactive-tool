async function getImageBlobFromUrl(url) {
  try {
    const fetchedImageData = await fetch(url);
    if (!fetchedImageData.ok) {
      throw new Error('Image fetch failed');
    }
    const blob = await fetchedImageData.blob();
    return blob;
  } catch (error) {
    console.error('Error fetching image:', error);
    throw error;
  }
}

$(document).ready(function () {
  // General function to handle image copying
  async function copyImage(buttonId, imageContainerId) {
    const src = $(`#${imageContainerId}>img`).attr("src");
    if (!src) {
      alert("No image source found!");
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
  }

  // Event listener for "stat_n_focus_line-copybtn"
  $("#stat_n_focus_line-copybtn").on("click", async () => {
    await copyImage("stat_n_focus_line-copybtn", "stat_n_focus_line-copy_plot");
  });

  // Event listener for "stat_n_multi_line-copybtn"
  $("#stat_n_multi_line-copybtn").on("click", async () => {
    await copyImage("stat_n_multi_line-copybtn", "stat_n_multi_line-copy_plot");
  });
});
