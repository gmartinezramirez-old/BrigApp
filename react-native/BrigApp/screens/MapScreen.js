
import React, { Component } from 'react';
import {
    AppRegistry,
    StyleSheet,
    View,
    Text,
    ToolbarAndroid
} from 'react-native';

var nativeImageSource = require('nativeImageSource');

import ActionButton from 'react-native-action-button';
import Icon from 'react-native-vector-icons/MaterialIcons';



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

        this.state = {
            areas: areas,
            layers: [],
            modoCommander: false,
            modoAvistamiento: false,
            titleToolbar: "",
        }
    }


    toModeCommander(){
        this.setState({modoCommander: true});
    }

    toModeAvistamiento(){
        console.log(">>  Modo Avistamiento");

        this.setState({
            modoAvistamiento: !this.state.modoAvistamiento,
            //titleToolbar: "Seleccione"
        });
    }

    // cancelo: false -> cancelado
    exitModeAvistamiento(cancelado){
        console.log("cancelado: " + cancelado);
        this.setState({
            modoAvistamiento: false,
            titleToolbar: ""
        });
    }

    getToolbar(title, onClickCancel, onClickOk){
        return (
            <ToolbarAndroid
                title={title}
                style={styles.toolbar}
                onIconClicked={() => onClickCancel(false)}
                navIcon={nativeImageSource({
                    android: 'ic_close_black_24dp',
                    width: 48,
                    height: 48
                })}
                logo={nativeImageSource({
                    android: 'ic_launcher',
                    width: 132,
                    height: 144
                })}
            />
        )
    }

    render(){
        const { navigate } = this.props.navigation;
        //var thecoords = coords;
        var thecoords = [];

        var toolbar = undefined;
        if(this.state.modoAvistamiento){
            toolbar = this.getToolbar(this.state.titleToolbar);
        }

        console.log("this.state.modoAvistamiento");
        console.log(this.state.modoAvistamiento);

        return (
            <View style ={styles.container}>
            <MapComponent
                    routes={thecoords}
                    areas={this.state.areas}
                    layers={this.state.layers}
                    toolbar={toolbar}
                >
                    <ActionButton
                        icon={<Icon name="layers" style={styles.actionButtonIcon} />}
                        buttonColor="rgba(231,76,60,1)"
                        onPress={() => {this.toModeAvistamiento()}}
                    />
                    <ActionButton
                        icon={<Icon name="directions" style={styles.actionButtonIcon} />}
                        buttonColor="rgba(240,240,240,1)"
                        onPress={() => {}}
                        offsetY={90}
                    />
                    <ActionButton
                        icon={<Icon name="directions" style={styles.actionButtonIcon} />}
                        buttonColor="rgba(240,240,240,1)"
                        onPress={() => {navigate('ChatRoomScreen')}}
                        offsetY={150}
                    />
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
        color: '#666',
    },
    toolbar: {
        backgroundColor: '#e9eaed',
        height: 56,
        zIndex: 1
    },
    map: {
        height: 100,
    }
});

AppRegistry.registerComponent('MapScreen', () => MapScreen);
