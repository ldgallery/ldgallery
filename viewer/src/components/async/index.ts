// Declare async constants for internal components
// Their name can't start with 'Ld'

export const MarkDown = () => import(/* webpackChunkName: "markdown" */ "./MarkDown.vue");
