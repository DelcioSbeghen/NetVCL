import { TControl } from './nv.controls.js';
import { ChangeObjProps } from './nv.classes.js';

export class TNvChart extends TControl {
    _DefaultParams(o) {
        o.Tag ??= "canvas";
        super._DefaultParams(o);
    }
    _CreateParams(o) {
        super._CreateParams(o);
        this.FConfig = {
            type: o.Type || "bar",
            data: {
                labels: [],
                datasets: []
            }
        };
        this.FCtx = this.FEl[0].getContext('2d');
        this.FChart = new Chart(this.FCtx, this.FConfig);
        this.FChart.update();
      }

    get Responsive() { return this.FConfig.options.responsive || false }
    set Responsive(V) {
        this.FRenderPosition = !V;
        if (V != this.FConfig.options.responsive) {
            this.FConfig.options.responsive = !V;
            this.FChart.update();
        }
    }

    get Config() { return this.FConfig }
    // set Config(V){ 
    //     if (V != this.FChart.config){
    //         this.FChart.config = V;
    //         this.FChart.update();  
    //     }
    // }

    get Type() { return this.FConfig.type }
    set Type(V) {
        if (V != this.FConfig.type) {
            this.FConfig.type = V;
            this.FChart.update();
        }
    }

    get Labels() { return this.FConfig.data.labels }
    set Labels(V) {
        if (V != this.FConfig.data.labels) {
            this.FConfig.data.labels = V;
            this.FChart.update();
        }
    }

    get Layout() { return this.FConfig.options.layout || {} }
    set Layout(V) {
        if (V != this.FConfig.options.layout) {
            ChangeObjProps(this.FConfig.options.layout, V);
            this.FChart.update();
        }
    }

    get Legend() { return this.FConfig.options.legend || {} }
    set Legend(V) {
        if (V != this.FConfig.options.legend) {
            ChangeObjProps(this.FConfig.options.legend, V)
            this.FChart.update();
        }
    }

    get Animation() { return this.FConfig.options.animation || {} }
    set Animation(V) {
        if (V != this.FConfig.options.animation) {
            ChangeObjProps(this.FConfig.options.animation, V)
            this.FChart.update();
        }
    }

    get Title() { return this.FConfig.options.title || {} }
    set Title(V) {
        if (V != this.FConfig.options.title) {
            ChangeObjProps(this.FConfig.options.title, V);
            this.FChart.update();
        }
    }

    get Elements() { return this.FConfig.options.elements || {} }
    set Elements(V) {
        if (V != this.FConfig.options.elements) {
            ChangeObjProps(this.FConfig.options.elements, V);
            this.FChart.update();
        }
    }

    get Tooltips() { return this.FConfig.options.tooltips || {} }
    set Tooltips(V) {
        if (V != this.FConfig.options.tooltips) {
            ChangeObjProps(this.FConfig.options.tooltips, V);
            this.FChart.update();
        }
    }

    get Data() { return this.FConfig.data }
    set Data(V) {
        if (V != this.FConfig.data) {
            if (V.datasets) {
                V.datasets.forEach((ds, i) => {
                    if (this.FConfig.data.datasets[i] === undefined)
                    this.FConfig.data.datasets.push(ds)
                    else
                        ChangeObjProps(this.FConfig.data.datasets[i], ds, ["_meta"])
                });
                delete V.datasets;
            }
            ChangeObjProps(this.FConfig.data, V, ["_meta"]);
            this.FChart.update();
        }
    }





    // get Options() { return this.FChart.options}
    // set Options(V) {
    //     if (V != this.FOptions) {
    //         this.FChart.config = V;
    //         this.FChart.update();
    //         this.FOptions = V;
    //     }
    // }
}

