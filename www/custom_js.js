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

  // Event listener for "Create your own line chart"
  $("#copybtn_line").on("click", async () => {
    await copyImage("copybtn_line", "copy_plot_line");
  });

  // Event listener for "Create your own bar chart"
  $("#copybtn_bar").on("click", async () => {
    await copyImage("copybtn_bar", "copy_plot_bar");
  });

    // Event listener for "Create your own line chart"
  $("#create_own_line-copybtn").on("click", async () => {
    await copyImage("create_own_line-copybtn", "create_own_line-copy_plot");
  });

    // Event listener for "Create your own line chart"
  $("#create_own_bar-copybtn").on("click", async () => {
    await copyImage("create_own_bar-copybtn", "create_own_bar-copy_plot");
  });

    // Event listener for "LA Level line chart"
  $("#la_line_chart-copybtn").on("click", async () => {
    await copyImage("la_line_chart-copybtn", "la_line_chart-copy_plot");
  });

    // Event listener for "LA Level bar chart"
  $("#la_bar_chart-copybtn").on("click", async () => {
    await copyImage("la_bar_chart-copybtn", "la_bar_chart-copy_plot");
  });

    // Event listener for "Stat Neigh focus line chart"
  $("#stat_n_focus_line-copybtn").on("click", async () => {
    await copyImage("stat_n_focus_line-copybtn", "stat_n_focus_line-copy_plot");
  });

    // Event listener for "Stat Neigh multi line chart"
  $("#stat_n_multi_line-copybtn").on("click", async () => {
    await copyImage("stat_n_multi_line-copybtn", "stat_n_multi_line-copy_plot");
  });
});
