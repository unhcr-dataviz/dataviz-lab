function animatePaths(pathInfo) {
    pathInfo.forEach((info, index) => {
      // Select the paths and set their initial 'stroke-dashoffset' to fully hide the paths
      d3.selectAll(info.paths)
        .attr('stroke-dasharray', function () {
          const length = this.getTotalLength();
          return `${length} ${length}`;
        })
        .attr('stroke-dashoffset', function () {
          const length = this.getTotalLength();
          return length;
        })
        .transition()
        .delay(info.delay)
        .duration(info.duration)
        .ease(d3.easeLinear)
        .attr('stroke-dashoffset', 0) // Animate stroke-dashoffset to reveal the path
        .on("end", () => {
          // Restart the animation if it's the last one in the array
          if (index === pathInfo.length - 1) {
            animatePaths(pathInfo);
          }
        });
    });
  }
  
  // Define the path information in an array
  const pathInformation1 = [
    { paths: "#path-1", delay: 0, duration: 1000 },
    { paths: "#path-2, #path-3, #path-4", delay: 900, duration: 2000 },
    { paths: "#path-8", delay: 1000, duration: 4000 },
    { paths: "#path-5, #path-6, #path-7, #path-9", delay: 3000, duration: 2000 },
    { paths: "#path-10, #path-11, #path-15, #path-17, #path-16, #path-19", delay: 5000, duration: 2000 },
    { paths: "#path-14, #path-18", delay: 7000, duration: 2000 },
    { paths: "#path-12, #path-13", delay: 7000, duration: 2000 },
  ];
  const pathInformation2 = [
    { paths: "#path-1s", delay: 0, duration: 1000 },
    { paths: "#path-2s, #path-3s, #path-4s", delay: 900, duration: 2000 },
    { paths: " #path-8s", delay: 1000, duration: 4000 },
    { paths: "#path-5s, #path-6s, #path-7s, #path-9s", delay: 3000, duration: 2000 },
    { paths: "#path-10s, #path-11s, #path-15s, #path-17s, #path-16s, #path-19s", delay: 5000, duration: 2000 },
    { paths: "#path-14s, #path-18s", delay: 7000, duration: 2000 },
    { paths: "#path-12s, #path-13s", delay: 7000, duration: 2000 },
  ];
  const pathInformation3 = [
    { paths: "#path-1m", delay: 0, duration: 1000 },
    { paths: "#path-2m, #path-3m, #path-4m", delay: 900, duration: 2000 },
    { paths: "#path-8, #path-8s, #path-8m", delay: 1000, duration: 4000 },
    { paths: "#path-5m, #path-6m, #path-7m, #path-9m", delay: 3000, duration: 2000 },
    { paths: "#path-10m, #path-11m, #path-15m, #path-17m, #path-16m, #path-19m", delay: 5000, duration: 2000 },
    { paths: "#path-14m, #path-18m", delay: 7000, duration: 2000 },
    { paths: "#path-12m, #path-13m", delay: 7000, duration: 2000 },
  ];
  // Start the animation
  animatePaths(pathInformation1);
  animatePaths(pathInformation2);
  animatePaths(pathInformation3)



  // Animation bubbles
  function animateBubbles() {
    d3.selectAll("[data-name='size'] path")
      .style("opacity", 0) // Set initial opacity to 0 (invisible)
      .transition()
      .delay(5000)
      .duration(6000)
      .style("opacity", 1) 
  }
  animateBubbles();