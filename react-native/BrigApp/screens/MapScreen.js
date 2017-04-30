
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


var personas = [
    {jefe: true,       pos: {longitude: -71.84916960332662 , latitude: -35.649353146812534 }, key: 1},
    {jefe: false, pos: {longitude: -71.84967060332662 , latitude: -35.649157546812534 }, key: 2},
    {jefe: false, pos: {longitude: -71.84936960332662 , latitude: -35.649853146812534 }, key: 3},
    {jefe: false, pos: {longitude: -71.84996560332662 , latitude: -35.649957946812534 }, key: 4},
    {jefe: false, pos: {longitude: -71.84985260332662 , latitude: -35.649553146812534 }, key: 5},
    {jefe: false, pos: {longitude: -71.84956160332662 , latitude: -35.649251146812534 }, key: 6},
]


function toLatLng(c){
    return { latitude: c[1], longitude: c[0],
    }
}

function toLatLngRev(c){
    return { latitude: c[0], longitude: c[1],
    }
}

export default class MapScreen extends Component {

    static navigationOptions = {
        header: null,
    }

    constructor(props){
        super(props);

        var inc = [[-35.64827916847159,-71.85047513593541],[-35.648369,-71.85047513593541],[-35.648458831528416,-71.85047513593541],[-35.64854866305683,-71.85036458267454],[-35.64863849458524,-71.85025402941366],[-35.64872832611365,-71.85025402941366],[-35.64881815764206,-71.85014347615278],[-35.648907989170475,-71.8500329228919],[-35.64899782069889,-71.84981181637015],[-35.6490876522273,-71.84970126310927],[-35.64917748375571,-71.84959070984839],[-35.64926731528412,-71.8494801565875],[-35.649357146812534,-71.84936960332662],[-35.64944697834095,-71.84925905006574],[-35.64953680986936,-71.84914849680487],[-35.649626641397774,-71.84914849680487],[-35.64971647292618,-71.84903794354399],[-35.64980630445459,-71.84881683702223],[-35.649896135983006,-71.84870628376135],[-35.64998596751142,-71.84859573050048],[-35.65007579903983,-71.8484851772396],[-35.65016563056824,-71.84837462397871],[-35.65016563056824,-71.84826407071783],[-35.65007579903983,-71.84815351745695],[-35.64998596751142,-71.84815351745695],[-35.649896135983006,-71.84826407071783],[-35.64980630445459,-71.84837462397871],[-35.64971647292618,-71.8484851772396],[-35.649626641397774,-71.84859573050048],[-35.64953680986936,-71.84870628376135],[-35.64944697834095,-71.84881683702223],[-35.649357146812534,-71.84892739028311],[-35.64926731528412,-71.84903794354399],[-35.64917748375571,-71.84914849680487],[-35.6490876522273,-71.84925905006574],[-35.64899782069889,-71.84936960332662],[-35.648907989170475,-71.8494801565875],[-35.64881815764206,-71.84959070984839],[-35.64872832611365,-71.84970126310927],[-35.64863849458524,-71.84981181637015],[-35.64854866305683,-71.84992236963102],[-35.648458831528416,-71.84992236963102],[-35.648369,-71.85014347615278],[-35.64827916847159,-71.85025402941366],[-35.64827916847159,-71.85025402941366]];

        console.log(inc.length);
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

        // pol. de poligono
        //pol1 = pol1.map(toLatLng)
        //incendio = {coordinates: pol1, color: "rgba(255,0,0,0.5)", id: 9, holes: []};
        //pol2 = pol2.map(toLatLng)
        //muchoPeligro = {coordinates: pol2, color: "rgba(255,154,64,0.5)", id: 10, holes: [pol1]};
        //pol3 = pol3.map(toLatLng)
        //pocoPeligro = {coordinates: pol3, color: "rgba(255,224,75, 0.5)", id: 11, holes:[pol2] , zIndex: 0};

        inc = inc.map(toLatLngRev)

        incendio = {coordinates: inc, color: "rgba(255,0,0,0.5)", id: 9, holes: []};


        areas = [ incendio];

        this.state = {
            areas: areas,
            layers: [],
            modoCommander: false,
            modoAvistamiento: false,
            titleToolbar: "",
            personas: personas,
        }
    }


    toModeCommander(){
        this.setState({modoCommander: true});
    }

    toModeAvistamiento(){

        this.setState({
            modoAvistamiento: !this.state.modoAvistamiento,
            //titleToolbar: "Seleccione"
        });
    }

    // cancelo: false -> cancelado
    exitModeAvistamiento(cancelado){
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


        return (
            <View style ={styles.container}>
            <MapComponent
                    routes={thecoords}
                    areas={this.state.areas}
                    layers={this.state.layers}
                    toolbar={toolbar}
                    personas={this.state.personas}
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
