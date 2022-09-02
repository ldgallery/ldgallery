
export const importFaIcon = async() => (await import(/* webpackChunkName: "icons" */ '@fortawesome/vue-fontawesome')).FontAwesomeIcon;
export const importHammer = async() => import(/* webpackChunkName: "hammer" */'hammerjs');
