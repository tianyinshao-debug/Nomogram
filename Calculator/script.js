// 预测模型的线性预测器系数
const linearPredictorCoefficients = {
  synchronousSplenectomy: { no: 0, yes: -0.641 },
  childPugh: { A: 0, B: 0.891 },
  afpLevel: { low: 0, high: 0.603 },
  tumorSize: { small: 0, large: 0.681 },
  gender: { female: 0, male: 0.625 },
  microvascularInvasion: { no: 0, yes: 0.865 }
};

// 预测公式系数
const predictionFormulas = {
  // Expected survival time = -4.6776×LP³ + 35.1357×LP² + -93.2287×LP + 100.8345
  survivalTime: {
    cubic: -4.6776,
    quadratic: 35.1357,
    linear: -93.2287,
    constant: 100.8345
  },
  // 3-years survival probability = 0.0313×LP³ + -0.1130×LP² + -0.2098×LP + 0.7873
  survival3Year: {
    cubic: 0.0313,
    quadratic: -0.1130,
    linear: -0.2098,
    constant: 0.7873
  },
  // 5-years survival probability = 0.0170×LP³ + -0.0116×LP² + -0.3403×LP + 0.6411
  survival5Year: {
    cubic: 0.0170,
    quadratic: -0.0116,
    linear: -0.3403,
    constant: 0.6411
  }
};

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

  // 更新风险等级显示（如果结果已显示）
  const riskLevelElement = document.getElementById('riskLevel');
  const resultsDiv = document.getElementById('results');
  if (riskLevelElement && !resultsDiv.classList.contains('hidden')) {
    const currentClass = riskLevelElement.className;
    if (currentClass.includes('low-risk')) {
      riskLevelElement.textContent = lang === 'zh' ? '低风险' : 'Low Risk';
    } else if (currentClass.includes('intermediate-risk')) {
      riskLevelElement.textContent = lang === 'zh' ? '中风险' : 'Intermediate Risk';
    } else if (currentClass.includes('high-risk')) {
      riskLevelElement.textContent = lang === 'zh' ? '高风险' : 'High Risk';
    }
  }

  // 保存语言偏好到localStorage
  localStorage.setItem('preferredLanguage', lang);
}

// 初始化页面语言
function initializeLanguage() {
  // 默认使用英文，因为用户要求默认语言为英文
  const savedLanguage = localStorage.getItem('preferredLanguage') || 'en';
  switchLanguage(savedLanguage);
}

// 计算线性预测器 (LP)
function calculateLinearPredictor(formData) {
  let lp = 0;

  for (const [factor, value] of Object.entries(formData)) {
    if (linearPredictorCoefficients[factor] && linearPredictorCoefficients[factor][value] !== undefined) {
      lp += linearPredictorCoefficients[factor][value];
    }
  }

  return Math.round(lp * 1000) / 1000; // 保留三位小数
}

// 计算预期生存时间
function calculateExpectedSurvivalTime(lp) {
  const formula = predictionFormulas.survivalTime;
  const result = formula.cubic * Math.pow(lp, 3) +
    formula.quadratic * Math.pow(lp, 2) +
    formula.linear * lp +
    formula.constant;

  return Math.max(0, Math.round(result * 10) / 10); // 保留一位小数，不能为负数
}

// 计算生存概率
function calculateSurvivalProbability(lp, years) {
  let formula;
  if (years === 3) {
    formula = predictionFormulas.survival3Year;
  } else if (years === 5) {
    formula = predictionFormulas.survival5Year;
  } else {
    return 0;
  }

  const result = formula.cubic * Math.pow(lp, 3) +
    formula.quadratic * Math.pow(lp, 2) +
    formula.linear * lp +
    formula.constant;

  // 转换为百分比并限制在0-100%之间
  const percentage = Math.max(0, Math.min(100, result * 100));
  return Math.round(percentage * 10) / 10; // 保留一位小数
}

// 计算风险分级
function calculateRiskLevel(riskScore) {
  if (riskScore >= -0.641 && riskScore < 1.478) {
    return {
      level: 'low-risk',
      labelEn: 'Low Risk',
      labelZh: '低风险'
    };
  } else if (riskScore >= 1.478 && riskScore < 1.740) {
    return {
      level: 'intermediate-risk',
      labelEn: 'Intermediate Risk',
      labelZh: '中风险'
    };
  } else if (riskScore >= 1.740 && riskScore <= 3.665) {
    return {
      level: 'high-risk',
      labelEn: 'High Risk',
      labelZh: '高风险'
    };
  } else {
    // 处理超出范围的情况
    if (riskScore < -0.641) {
      return {
        level: 'low-risk',
        labelEn: 'Low Risk',
        labelZh: '低风险'
      };
    } else {
      return {
        level: 'high-risk',
        labelEn: 'High Risk',
        labelZh: '高风险'
      };
    }
  }
}

// 显示结果
function displayResults(results) {
  const resultsDiv = document.getElementById('results');
  const noResultsDiv = document.getElementById('noResults');

  // 更新风险评估
  document.getElementById('riskScore').textContent = results.riskScore;
  const riskLevelElement = document.getElementById('riskLevel');
  const riskLevel = results.riskLevel;

  // 设置风险等级文本和样式
  riskLevelElement.textContent = currentLanguage === 'zh' ? riskLevel.labelZh : riskLevel.labelEn;
  riskLevelElement.className = `risk-badge ${riskLevel.level}`;

  // 更新生存数据
  document.getElementById('expectedSurvival').textContent = results.expectedSurvivalTime;
  document.getElementById('survival3Year').textContent = results.survival3Year + '%';
  document.getElementById('survival5Year').textContent = results.survival5Year + '%';

  // 显示结果，隐藏提示
  resultsDiv.classList.remove('hidden');
  noResultsDiv.classList.add('hidden');

  // 添加动画效果
  resultsDiv.style.opacity = '0';
  setTimeout(() => {
    resultsDiv.style.transition = 'opacity 0.5s ease-in';
    resultsDiv.style.opacity = '1';
  }, 100);
}

// 验证表单
function validateForm(formData) {
  const requiredFields = [
    'synchronousSplenectomy',
    'childPugh',
    'afpLevel',
    'tumorSize',
    'gender',
    'microvascularInvasion'
  ];

  for (const field of requiredFields) {
    if (!formData[field]) {
      return false;
    }
  }
  return true;
}

// 收集表单数据
function collectFormData() {
  const calculatorForm = document.getElementById('calculatorForm');
  const formData = new FormData(calculatorForm);
  const data = {};

  for (const [key, value] of formData.entries()) {
    data[key] = value;
  }

  return data;
}

// 添加表单提交事件监听
document.addEventListener('DOMContentLoaded', function () {
  // 初始化页面语言
  initializeLanguage();

  const form = document.getElementById('calculatorForm');

  form.addEventListener('submit', function (e) {
    e.preventDefault();

    // 收集表单数据
    const formData = collectFormData();

    // 验证表单
    if (!validateForm(formData)) {
      const message = currentLanguage === 'zh' ? '请填写所有必需的字段' : 'Please fill in all required fields';
      alert(message);
      return;
    }

    // 添加计算动画
    const calculateBtn = document.querySelector('.calculate-btn');
    const originalText = calculateBtn.textContent;
    const calculatingText = currentLanguage === 'zh' ? '计算中...' : 'Calculating...';
    calculateBtn.textContent = calculatingText;
    calculateBtn.disabled = true;

    // 模拟计算延迟
    setTimeout(() => {
      // 计算线性预测器（风险指数）
      const lp = calculateLinearPredictor(formData);

      // 计算风险分级
      const riskLevel = calculateRiskLevel(lp);

      // 计算预期生存时间
      const expectedSurvivalTime = calculateExpectedSurvivalTime(lp);

      // 计算生存概率
      const survival3Year = calculateSurvivalProbability(lp, 3);
      const survival5Year = calculateSurvivalProbability(lp, 5);

      // 组织结果
      const results = {
        linearPredictor: lp,
        riskScore: lp,
        riskLevel: riskLevel,
        expectedSurvivalTime: expectedSurvivalTime,
        survival3Year: survival3Year,
        survival5Year: survival5Year
      };

      // 显示结果
      displayResults(results);

      // 恢复按钮
      calculateBtn.textContent = originalText;
      calculateBtn.disabled = false;

      // 滚动到结果区域
      document.querySelector('.results-section').scrollIntoView({
        behavior: 'smooth'
      });

    }, 1000); // 1秒延迟模拟计算过程
  });

  // 添加单选按钮点击效果
  const radioOptions = document.querySelectorAll('.radio-option');
  radioOptions.forEach(option => {
    option.addEventListener('click', function () {
      const radio = this.querySelector('input[type="radio"]');
      if (radio) {
        radio.checked = true;

        // 移除同组其他选项的选中状态视觉效果
        const groupName = radio.name;
        const sameGroupOptions = document.querySelectorAll(`input[name="${groupName}"]`);
        sameGroupOptions.forEach(otherRadio => {
          otherRadio.closest('.radio-option').classList.remove('selected');
        });

        // 添加当前选项的选中状态
        this.classList.add('selected');
      }
    });
  });

  // 添加输入验证提示
  const validationForm = document.getElementById('calculatorForm');
  const inputs = validationForm.querySelectorAll('input[required]');

  inputs.forEach(input => {
    input.addEventListener('change', function () {
      // 检查是否所有必需字段都已填写
      const allFilled = Array.from(inputs).every(inp => {
        const groupName = inp.name;
        return document.querySelector(`input[name="${groupName}"]:checked`);
      });

      if (allFilled) {
        document.querySelector('.calculate-btn').style.background =
          'linear-gradient(135deg, #27ae60 0%, #2ecc71 100%)';
      }
    });
  });
});

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
  // Ctrl+Enter 提交表单
  if (e.ctrlKey && e.key === 'Enter') {
    const submitForm = document.getElementById('calculatorForm');
    submitForm.dispatchEvent(new Event('submit'));
  }

  // ESC 清除结果
  if (e.key === 'Escape') {
    const resultsDiv = document.getElementById('results');
    const noResultsDiv = document.getElementById('noResults');

    resultsDiv.classList.add('hidden');
    noResultsDiv.classList.remove('hidden');
  }
});

// 导出结果功能
function exportResults() {
  const results = document.getElementById('results');
  if (results.classList.contains('hidden')) {
    alert('请先计算结果');
    return;
  }

  // 这里可以添加导出为PDF或打印的功能
  window.print();
}

// 重置表单功能
function resetForm() {
  const resetTargetForm = document.getElementById('calculatorForm');
  resetTargetForm.reset();

  // 隐藏结果
  const resultsDiv = document.getElementById('results');
  const noResultsDiv = document.getElementById('noResults');

  resultsDiv.classList.add('hidden');
  noResultsDiv.classList.remove('hidden');

  // 移除所有选中状态
  const radioOptions = document.querySelectorAll('.radio-option');
  radioOptions.forEach(option => {
    option.classList.remove('selected');
  });
}
