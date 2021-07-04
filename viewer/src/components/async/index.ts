// Declare async constants for internal components
// Their name can't start with 'Ld'

export const Markdown = () => import(/* webpackChunkName: "markdown" */ "./Markdown.vue");
