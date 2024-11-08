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
  async function copyImage(namespace) {
    const buttonId = `#${namespace}-copybtn`;
    const imageContainerId = `#${namespace}-copy_plot`;

    const src = $(`${imageContainerId}>img`).attr("src");
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
      Shiny.setInputValue("copy-to-clipboard-success", true, {priority: "event"});
    } catch (err) {
      console.error(err.name, err.message);
      Shiny.setInputValue("copy-to-clipboard-failure", true, {priority: "event"});
    }
  }

  // Event listener for all copy buttons
  const namespaces = [
    "stat_n_focus_line", "stat_n_multi_line", "copybtn_line", "copybtn_bar",
    "create_own_line", "create_own_bar", "la_line_chart", "la_bar_chart",
    "stat_n_focus_line", "stat_n_multi_line", "stat_n_focus_bar", "stat_n_multi_bar",
    "region_focus_line", "region_multi_line", "region_focus_bar", "region_multi_bar"
  ];

  namespaces.forEach(namespace => {
    $(`#${namespace}-copybtn`).on("click", async () => {
      await copyImage(namespace);
    });
  });
});
