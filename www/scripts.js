function download_plotly(plotly_id, filename, width = 1000, height = 500){
    var gd = document.getElementById(plotly_id);
    Plotly.toImage(gd, {format: "jpeg", width: width, height: height}).then(function(url) {
    var a = window.document.createElement("a");
    a.href = url; 
    a.type = "image/jpeg";
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);                      
    });
}

document.getElementById("download_box_count_bridge").onclick = function() {
    download_plotly("Mean_Count_Bridging", "bead_counts_bridge.jpeg");
}

document.getElementById("download_box_count_sample").onclick = function() {
    download_plotly("Mean_Count_Sample","bead_counts_sample.jpeg");
}

document.getElementById("download_blank_sample").onclick = function() {
    download_plotly("Blank_Sample","blank_values_sample.jpeg");
}

document.getElementById("download_blank_bridge").onclick = function() {
    download_plotly("Blank_Bridging","blank_values_bridge.jpeg");
}

document.getElementById("download_deltaT").onclick = function() {
    download_plotly("DeltaT_Combined","temperature.jpeg", 800, 800);
}

document.getElementById("download_KT3_bridge").onclick = function() {
    download_plotly("KT3_Bridge","KT3_bridge.jpeg", 800, 800);
}
document.getElementById("download_GST_sample").onclick = function() {
    download_plotly("GST_Sample","GST_sample.jpeg");
}

document.getElementById("download_GST_bridge").onclick = function() {
    download_plotly("GST_Bridge","GST_bridge.jpeg");
}

document.getElementById("download_MFI_bridge_mean").onclick = function() {
    download_plotly("Mean_MFI_Bridging","mean_MFI_bridge.jpeg");
}

document.getElementById("download_MFI_bridge_median").onclick = function() {
    download_plotly("Median_MFI_Bridging","median_MFI_bridge.jpeg");
}

document.getElementById("download_MFI_perplate_mean").onclick = function() {
    download_plotly("Mean_MFI_perplate","mean_MFI_bridge_pp.jpeg");
}

document.getElementById("download_MFI_perplate_median").onclick = function() {
    download_plotly("Median_MFI_perplate","median_MFI_bridge_pp.jpeg");
}

document.getElementById("download_control_1").onclick = function() {
    download_plotly("control_1","control_1.jpeg");
}

document.getElementById("download_control_2").onclick = function() {
    download_plotly("control_2","control_2.jpeg");
}

document.getElementById("download_control_3").onclick = function() {
    download_plotly("control_3","control_3.jpeg");
}
