window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());

gtag('config', 'your-id');

$(window).on('load resize', function() {
  if($(window).width() < 768) {
    document.getElementsByClassName('menu-strip')[0].style.display ="none";
    document.getElementsByTagName('nav')[0].style.display ="";
    document.getElementsByClassName('drawer-toggle')[0].style.display ="";
  } else {
    document.getElementsByClassName('menu-strip')[0].style.display ="";
    document.getElementsByTagName('nav')[0].style.display ="none";
    document.getElementsByClassName('drawer-toggle')[0].style.display ="none";
  }
});

$(document).ready(function() {
  $("body").addClass("drawer drawer--right");
  $("nav").addClass("drawer-nav");
  $("nav > h2").each(function(index, element){
    if ($(element).next("p")) {
      if ($(element).next("p").children().length == 0) {
        $(element).next("p").remove()
      }
    }

    let $ul = $(element).next("ul");
    if ($ul.length) {
      $(element).addClass("drawer-menu-item");
      $(element).attr("data-toggle", 'dropdown');
      $(element).attr("role", 'button');
      $(element).attr("aria-expanded", 'false');
      $(element).wrap('<li class="drawer-dropdown" />');
      $(element).append('<span class="drawer-caret"></span>');
      $ul.addClass("drawer-dropdown-menu");
      $ul.find("a").addClass("drawer-dropdown-menu-item");
      $ul.insertAfter($(element));
    }
  })
  $("nav").children().wrapAll('<ul class="drawer-menu" />');
  $('.drawer').drawer();
  lazyload();

  $("nav a").attr("target", '_self');
  $(".menu-strip a").attr("target", '_self');

  let appear = false;
  const pagetop = $('#page_top');
  $(window).scroll(function () {
    if ($(this).scrollTop() > 150) {
      if (appear == false) {
        appear = true;
        let bottom = "";
        if($(window).width() < 768) {
          bottom = "10px";
        } else {
          bottom = "90px";
        }
        pagetop.stop().animate({ 'bottom': bottom }, 500);
      }
    } else {
      if (appear) {
        appear = false;
        pagetop.stop().animate({ 'bottom': '-100px' }, 500);
      }
    }
  });
  pagetop.click(function () {
    $('body, html').animate({ scrollTop: 0 }, 500);
    return false;
  });
});
