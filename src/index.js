import './font_roboto.css';
import './font_material_icons.css';
import './material.teal-red.min.css';
import './main.css';

import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

Main.embed(document.getElementById('root'));

registerServiceWorker();
