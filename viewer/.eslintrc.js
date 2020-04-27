module.exports = {
  root: true,

  env: {
    node: true,
  },

  extends: [
    "plugin:vue/essential",
    "@vue/typescript"
  ],

  rules: {
    "no-console": "off",
    "no-debugger": process.env.NODE_ENV === "production" ? "error" : "off",
    "eol-last": ["warn", "always"],
    "object-curly-spacing": ["warn", "always"],
    "indent": ["warn", 2, { "SwitchCase": 1 }],
    "quote-props": ["warn", "as-needed"],
    "quotes": ["warn", "double"],
    "vue/attribute-hyphenation": "warn",
    "vue/html-closing-bracket-spacing": "warn",
    "vue/html-end-tags": "error",
    "vue/html-quotes": "warn",
    "vue/html-self-closing": "off",
    "vue/no-multi-spaces": "warn",
    "vue/no-spaces-around-equal-signs-in-attribute": "warn",
    "vue/no-template-shadow": "error",
    "vue/v-bind-style": "warn",
    "vue/v-on-style": "warn",
    "vue/attributes-order": "warn",
    "vue/this-in-template": "warn",
  },

  parserOptions: {
    sourceType: "module",
    parser: "@typescript-eslint/parser",
  },

};
