// 当前语言状态
let currentLanguage = 'en'; // 默认英文

// 语言切换功能
function switchLanguage(lang) {
  const langButtons = document.querySelectorAll('.lang-btn');
  langButtons.forEach(btn => btn.classList.remove('active'));

  // 更简单的方法：直接通过按钮文本内容找到对应按钮
  langButtons.forEach(btn => {
    if ((lang === 'zh' && btn.textContent === '中文') ||
      (lang === 'en' && btn.textContent === 'English')) {
      btn.classList.add('active');
    }
  });

  currentLanguage = lang;

  // 获取所有具有双语属性的元素
  const elements = document.querySelectorAll('[data-en][data-zh]');

  elements.forEach(element => {
    if (lang === 'en') {
      element.textContent = element.getAttribute('data-en');
    } else if (lang === 'zh') {
      element.textContent = element.getAttribute('data-zh');
    }
  });

  // 更新页面标题
  const titleElement = document.querySelector('title');
  if (titleElement) {
    if (lang === 'en') {
      document.title = titleElement.getAttribute('data-en') || document.title;
    } else if (lang === 'zh') {
      document.title = titleElement.getAttribute('data-zh') || document.title;
    }
  }

  // 更新HTML语言属性
  document.documentElement.lang = lang === 'zh' ? 'zh-CN' : 'en';

  // 保存语言偏好到localStorage
  localStorage.setItem('preferredLanguage', lang);
}

// 初始化页面语言
function initializeLanguage() {
  // 默认使用英文，因为用户要求默认语言为英文
  const savedLanguage = localStorage.getItem('preferredLanguage') || 'en';
  switchLanguage(savedLanguage);
}

// 访问计数功能
function updateVisitCounter() {
  // 从localStorage获取访问次数
  let visitCount = localStorage.getItem('visitCount') || '0';
  visitCount = parseInt(visitCount) + 1;

  // 更新计数
  localStorage.setItem('visitCount', visitCount.toString());

  // 显示计数（添加动画效果）
  const countElement = document.getElementById('visitCount');
  if (countElement) {
    countElement.textContent = visitCount.toLocaleString();

    // 添加计数动画
    countElement.style.transform = 'scale(1.2)';
    countElement.style.color = '#e74c3c';

    setTimeout(() => {
      countElement.style.transform = 'scale(1)';
      countElement.style.color = '#3498db';
    }, 300);
  }
}

// 卡片hover效果增强
function enhanceCardEffects() {
  const cards = document.querySelectorAll('.calculator-card');

  cards.forEach(card => {
    card.addEventListener('mouseenter', function () {
      // 为其他卡片添加轻微的暗化效果
      cards.forEach(otherCard => {
        if (otherCard !== this) {
          otherCard.style.opacity = '0.7';
        }
      });
    });

    card.addEventListener('mouseleave', function () {
      // 恢复所有卡片的透明度
      cards.forEach(otherCard => {
        otherCard.style.opacity = '1';
      });
    });
  });
}

// 页面加载完成后的初始化
document.addEventListener('DOMContentLoaded', function () {
  // 初始化语言
  initializeLanguage();

  // 更新访问计数
  updateVisitCounter();

  // 增强卡片效果
  enhanceCardEffects();

  // 添加按钮点击效果
  const calcButtons = document.querySelectorAll('.calc-btn');
  calcButtons.forEach(btn => {
    btn.addEventListener('click', function (e) {
      // 添加点击波纹效果
      const ripple = document.createElement('span');
      ripple.style.cssText = `
                position: absolute;
                border-radius: 50%;
                background: rgba(255,255,255,0.6);
                transform: scale(0);
                animation: ripple 0.6s linear;
                pointer-events: none;
            `;

      const rect = this.getBoundingClientRect();
      const size = Math.max(rect.width, rect.height);
      ripple.style.width = ripple.style.height = size + 'px';
      ripple.style.left = (e.clientX - rect.left - size / 2) + 'px';
      ripple.style.top = (e.clientY - rect.top - size / 2) + 'px';

      this.style.position = 'relative';
      this.appendChild(ripple);

      setTimeout(() => {
        ripple.remove();
      }, 600);
    });
  });
});

// 添加CSS动画规则
const style = document.createElement('style');
style.textContent = `
    @keyframes ripple {
        to {
            transform: scale(4);
            opacity: 0;
        }
    }
`;
document.head.appendChild(style);

// 添加页面加载动画
window.addEventListener('load', function () {
  const container = document.querySelector('.container');
  container.style.opacity = '0';
  container.style.transform = 'translateY(30px)';

  setTimeout(() => {
    container.style.transition = 'all 0.8s ease-out';
    container.style.opacity = '1';
    container.style.transform = 'translateY(0)';
  }, 100);
});

// 添加滚动效果
window.addEventListener('scroll', function () {
  const scrolled = window.pageYOffset;
  const rate = scrolled * -0.5;

  const header = document.querySelector('.header');
  if (header) {
    header.style.transform = `translateY(${rate}px)`;
  }
});

// 键盘快捷键支持
document.addEventListener('keydown', function (e) {
  // Alt+H 返回主页（如果在子页面）
  if (e.altKey && e.key === 'h') {
    window.location.href = 'index.html';
  }

  // 数字键1-8快速访问对应计算器
  if (e.key >= '1' && e.key <= '8') {
    const calculators = [
      'hbv-hcc.html',
      'early-recurrence.html',
      'bclc-0a-hcc-survival.html',
      'morbidity-risk.html',
      'transfusion-risk.html',
      'liver-failure.html',
      'mortality-6m.html',
      'complications-chb.html'
    ];

    const index = parseInt(e.key) - 1;
    if (calculators[index]) {
      window.location.href = calculators[index];
    }
  }
});

// 搜索功能（可选）
function searchCalculators(query) {
  const cards = document.querySelectorAll('.calculator-card');
  const searchTerm = query.toLowerCase();

  cards.forEach(card => {
    const title = card.querySelector('h3').textContent.toLowerCase();
    const description = card.querySelector('p').textContent.toLowerCase();

    if (title.includes(searchTerm) || description.includes(searchTerm)) {
      card.style.display = 'block';
      card.style.animation = 'fadeIn 0.5s ease-out';
    } else {
      card.style.display = 'none';
    }
  });
}

// 导出功能
function exportCalculatorList() {
  const calculators = Array.from(document.querySelectorAll('.calculator-card')).map(card => {
    return {
      title: card.querySelector('h3').textContent,
      description: card.querySelector('p').textContent,
      link: card.querySelector('.calc-btn').href
    };
  });

  const dataStr = JSON.stringify(calculators, null, 2);
  const dataBlob = new Blob([dataStr], { type: 'application/json' });
  const url = URL.createObjectURL(dataBlob);

  const link = document.createElement('a');
  link.href = url;
  link.download = 'calculator-list.json';
  link.click();

  URL.revokeObjectURL(url);
}
