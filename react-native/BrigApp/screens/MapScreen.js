
import React, { Component } from 'react';
import {
    AppRegistry,

    StyleSheet,
    View,
    Text
} from 'react-native';

import MapComponent from '../Components/MapComponents';


var coords = [ [ -70.660088, -33.416625 ], [ -70.656752, -33.416566 ], [ -70.656395, -33.417651 ], [ -70.651693, -33.417599 ], [ -70.650541, -33.420073 ], [ -70.65051, -33.419904 ] ];


function toLatLng(c){
    return { latitude: c[1], longitude: c[0],
    }
}

export default class MapScreen extends Component {

    static navigationOptions = {
        header: null,
    }

    constructor(props){
        super(props);

        var pol1 = [
            [-70.660000, -33.416000], // izq-arriba
            [-70.663000, -33.417500], 
            [-70.660000, -33.419000], // izq-abajo
            [-70.657500, -33.419800], 
            [-70.655000, -33.419000], // derecha-abajo
            [-70.655000, -33.416000], // derecha-arriba
        ];
        var pol2 = [
            [-70.660200, -33.415000], // izq-arriba
            [-70.664000, -33.417500], 
            [-70.660200, -33.420000], // izq-abajo
            [-70.657500, -33.420800], 
            [-70.650000, -33.419000], // derecha-abajo
            [-70.650000, -33.415000], // derecha-arriba
        ];

        var pol3 = [
            [-70.660400, -33.414000], // izq-arriba
            [-70.665600, -33.417500], 
            [-70.660400, -33.422000], // izq-abajo
            [-70.657500, -33.422800], 
            [-70.645000, -33.419000], // derecha-abajo
            [-70.645000, -33.414000], // derecha-arriba
        ];
        var coordHumo = [
            [-70.660400, -33.414000], // izq-arriba
            [-70.665600, -33.417500], 
            [-70.660400, -33.422000], // izq-abajo
            [-70.645000, -33.419000], // derecha-abajo
            [-70.645000, -33.414000], // derecha-arriba
            [-70.652700, -33.410000], 
        ]

        var coordCenizas = [
            [-70.660400, -33.414000], // izq-arriba
            [-70.665600, -33.417500], 
            [-70.660400, -33.422000], // izq-abajo
            [-70.645000, -33.419000], // derecha-abajo
            [-70.645000, -33.414000], // derecha-arriba
            [-70.652700, -33.412000], 
        ]

        coords = coords.map(toLatLng);
        coords = [{coordinates: coords, color: "#000", id: 0}];

        pol1 = pol1.map(toLatLng)
        incendio = {coordinates: pol1, color: "rgba(255,0,0,0.5)", id: 9, holes: []};

        pol2 = pol2.map(toLatLng)
        muchoPeligro = {coordinates: pol2, color: "rgba(255,154,64,0.5)", id: 10, holes: [pol1]};

        pol3 = pol3.map(toLatLng)

        pocoPeligro = {coordinates: pol3, color: "rgba(255,224,75, 0.5)", id: 11, holes:[pol2] , zIndex: 0};


        areas = [pocoPeligro, muchoPeligro, incendio];

        // las capas
        coordHumo = coordHumo.map(toLatLng);
        coordCenizas = coordCenizas.map(toLatLng);
        
        //humo    = {coordinates: coordHumo, color: "rgba(80,80,80, 0.5)", id: 12, holes:[], zIndex: 9999999};
        //cenizas = {coordinates: coordCenizas, color: "rgba(200,200,200, 0.5)", id: 13, holes:[], zIndex: 9999995};
        //humo = {name: "Humo", defaultVisible: true, polygons: [humo]};
        //cenizas = {name: "Cenizas", defaultVisible: false, polygons: [cenizas]};

        //layers = [humo, cenizas];

        this.state = {
            areas: areas,
            layers: []
        }
    }

    render(){
        const { navigate } = this.props.navigation;
        //var thecoords = coords;
        var thecoords = [];
        return (
            <View style ={styles.container}>
                <MapComponent
                    routes={thecoords}
                    areas={this.state.areas}
                    layers={this.state.layers}
                >
                </MapComponent>
            </View>
        )
    }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    backgroundColor: '#F5FCFF',
  },

  actionButtonIcon: {
    fontSize: 20,
    height: 22,
    color: 'white',
  },
});

AppRegistry.registerComponent('MapScreen', () => MapScreen);
