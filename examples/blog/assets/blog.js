window.dataLayer = window.dataLayer || [];
function gtag() { dataLayer.push(arguments); }
gtag('js', new Date());
gtag('config', 'your-id');

document.addEventListener('DOMContentLoaded', () => {
  // lazyload() が外部ライブラリの場合はそのまま呼び出し
  if (typeof lazyload === 'function') {
    lazyload();
  }

  // リンクのターゲット設定
  const setTargetSelf = (selector) => {
    document.querySelectorAll(selector).forEach(a => a.setAttribute('target', '_self'));
  };
  setTargetSelf('nav a');
  setTargetSelf('.menu-strip a');

  // トップへ戻るボタンの制御
  const pagetop = document.getElementById('page_top');
  if (pagetop) {
    let appear = false;

    // スクロール時の挙動
    window.addEventListener('scroll', () => {
      const scrollY = window.scrollY;
      if (scrollY > 150) {
        if (!appear) {
          appear = true;
          const isSP = window.matchMedia("(max-width: 1024px) and (orientation: portrait), (max-width: 768px)").matches;
          const bottom = isSP ? "10px" : "90px";
          pagetop.style.transition = "bottom 0.5s ease";
          pagetop.style.bottom = bottom;
        }
      } else {
        if (appear) {
          appear = false;
          pagetop.style.transition = "bottom 0.5s ease";
          pagetop.style.bottom = "-100px";
        }
      }
    });

    // クリック時のスムーズスクロール
    pagetop.addEventListener('click', (e) => {
      e.preventDefault();
      window.scrollTo({ top: 0, behavior: 'smooth' });
    });
  }
});
