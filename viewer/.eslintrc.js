module.exports = {
  root: true,

  env: {
    node: true,
  },

  'extends': [
    'plugin:vue/essential',
    '@vue/typescript'
  ],

  rules: {
    "no-console": process.env.NODE_ENV === "production" ? "error" : "off",
    "no-debugger": process.env.NODE_ENV === "production" ? "error" : "off",
    'vue/attribute-hyphenation': 'warn',
    'vue/html-closing-bracket-spacing': 'warn',
    'vue/html-end-tags': 'error',
    'vue/html-quotes': 'warn',
    'vue/html-self-closing': 'warn',
    'vue/no-multi-spaces': 'warn',
    'vue/no-spaces-around-equal-signs-in-attribute': 'warn',
    'vue/no-template-shadow': 'error',
    'vue/v-bind-style': 'warn',
    'vue/v-on-style': 'warn',
    'vue/attributes-order': 'warn',
    'vue/this-in-template': 'warn'
  },

  parserOptions: {
    parser: '@typescript-eslint/parser',
  },

};
