
  <!DOCTYPE html>
  <html>
  <head>
  <meta name="viewport" content="width=device-width, initial-scale=1">
    <style>
    * {box-sizing:border-box}
  .mySlides {display:none
    width: 575px;
    height: 365px;
    position: relative;
    margin: auto;
  }
  
  /* Slideshow container */
    .slideshow-container {
      width: 575px;
      height: 365px;
      position: relative;
      margin: auto;
    }
  
  /* Caption text */
    .text {
      color: #f2f2f2;
        font-size: 75%;
      position: absolute;
      bottom: 8px;
      width: 575px;
      text-align: center;
    }
  
  /* Fading animation */
    .fade {
      -webkit-animation-name: fade;
      -webkit-animation-duration: 1s;
      animation-name: fade;
      animation-duration: 8s;
      position: relative;
      margin: auto;
    }
  
  @-webkit-keyframes fade {
    from {opacity: 0.7} 
    to {opacity: 1}
  }
  
  @keyframes fade {
    from {opacity: 0.7} 
    to {opacity: 1}
  }
  
  /* On smaller screens, decrease text size */
    @media only screen and (max-width: 300px) {
      .text {font-size: 11px}
    }
  </style>
    </head>
    <body>
    
    <div class="slideshow-container" align="center">
    <center>
    <div class="mySlides fade">
    <img src="/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/figures/IMG_0062.jpg">
    <div class="text" text-align: center>*United States Capitol, 113^th^ Congress.*</div>
    </div>
    
    <div class="mySlides fade">
    <img src="/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/figures/house_republicans.png">
    </div>
    
    <div class="mySlides fade">
    <img src="/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/figures/10.jpg">
    </div>
    
    <div class="mySlides fade">
    <img src="/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/figures/obstruction_115.png">
    </div>
    
    <div class="mySlides fade">
    <img src="/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/figures/17.png">
    </div>
    
    <div class="mySlides fade">
    <img src="/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/figures/4.png">
    </div>
    
    <div class="mySlides fade">
    <img src="/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/figures/13.png">
    </div>
    
    <div class="mySlides fade">
    <img src="/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/figures/11.png">
    </div>
    
    <div class="mySlides fade">
    <img src="/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/figures/3_0.png">
    </div>
    
    <div class="mySlides fade">
    <img src="/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/figures/7.png">
    </div>
    
    <div class="mySlides fade">
    <img src="/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/figures/12.png">
    </div>
    
    <div class="mySlides fade">
    <img src="/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/figures/3.png">
    </div>
    
    <div class="mySlides fade">
    <img src="/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/figures/4_2.png">
    </div>
    
    <div class="mySlides fade">
    <img src="/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/figures/4_1.png">
    </div>
    
    <div class="mySlides fade">
    <img src="/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/figures/15.png">
    </div>
    
    </center>
    
    </div>
    <br>
    
    <div style="text-align:center">
    <span class="dot"></span> 
    <span class="dot"></span> 
    <span class="dot"></span>
    <span class="dot"></span> 
    <span class="dot"></span> 
    <span class="dot"></span> 
    <span class="dot"></span> 
    <span class="dot"></span> 
    <span class="dot"></span> 
    <span class="dot"></span> 
    <span class="dot"></span> 
    <span class="dot"></span> 
    <span class="dot"></span> 
    <span class="dot"></span> 
    <span class="dot"></span> 
    
    </div>
    
    <script>
    var slideIndex = 0;
  showSlides();
  
  function showSlides() {
    var i;
    var slides = document.getElementsByClassName("mySlides");
    var dots = document.getElementsByClassName("dot");
    for (i = 0; i < slides.length; i++) {
      slides[i].style.display = "none";  
    }
    slideIndex++;
    if (slideIndex> slides.length) {slideIndex = 1}    
    for (i = 0; i < dots.length; i++) {
      dots[i].className = dots[i].className.replace(" active", "");
    }
    slides[slideIndex-1].style.display = "block";  
    dots[slideIndex-1].className += " active";
    setTimeout(showSlides, 8000); // Change image every 8 seconds
  }
  </script>
    
    </body>
    </html> 