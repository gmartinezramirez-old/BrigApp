
import React, { Component } from 'react';
import {
    AppRegistry,
    StyleSheet,
    Text,
    View,
    TouchableOpacity,
    Modal,
    ToolbarAndroid
} from 'react-native';
import {
    Button,
    CheckboxGroup,
    Subheader
} from 'react-native-material-design';

import MapView from 'react-native-maps';
var nativeImageSource = require('nativeImageSource');

import ActionButton from 'react-native-action-button';
import Icon from 'react-native-vector-icons/MaterialIcons';

let id = 0;
const STROKE_WIDTH = 3;

// Layer:
//  {
//    defaultVisible: bool,
//    polygons: [[ {id: number, coordinates: , holes:, color:} ]]
//    name: string
//    visible: bool
//  }
//  layers: [Layer]
export default class MapComponent extends Component {

    constructor(props) {
        super(props);

        var layers = [];
        if(props.layers !== undefined){
            layers = props.layers.map(function(e){
                e.visible = e.defaultVisible;
                return e;
            });
        }

        this.state = {
            lat: -35.648369,    // lugar incendio
            lng: -71.850586,
            //lat: -33.416625,  // mi casa - pato ;D
            //lng: -70.660088,
            polygons: [],
            lines: [],
            editing: null,
            creatingHole: false,
            modalVisible: false,
            layers: layers, // guarda todas las capas
        }
    }

    componentDidMount() {
        // obtiene la posicion
        //navigator.geolocation.getCurrentPosition(
            //(position) => {
                //var initialPosition = JSON.stringify(position); 
                //this.setState({initialPosition});
            //},
            //(error) => console.log("error: " + error),
            //{enableHighAccuracy: true, timeout: 20000, maximumAge: 1000}
        //); 
        //this.watchID = navigator.geolocation.watchPosition((position) => {
            //this.setState({
                //lat: position.coords.latitude,
                //lng: position.coords.longitude,
            //});
        //});
    }

    // cuando se termina de crear un poligono o linea
    finish(canCreatePolygon, canCreateLine) {
        if(canCreatePolygon){
            const { polygons, editing } = this.state;
            this.setState({
                polygons: [...polygons, editing],
                editing: null,
                creatingHole: false,
            });
        }else if(canCreateLine){
            // cuando se termina de crear una linea
            const { lines, editing } = this.state;
            this.setState({
                lines: [...lines, editing],
                editing: null,
            });
        }
    }

    createHole() {
        const { editing, creatingHole } = this.state;
        if (!creatingHole) {
            this.setState({
                creatingHole: true,
                editing: {
                    ...editing,
                    holes: [
                        ...editing.holes,
                        [],
                    ],
                },
            });
        } else {
            const holes = [...editing.holes];
            if (holes[holes.length - 1].length === 0) {
                holes.pop();
                this.setState({
                    editing: {
                        ...editing,
                        holes,
                    },
                });
            }
            this.setState({ creatingHole: false });
        }
    }

    creatingPolygon(e){
        const { editing, creatingHole } = this.state;
        if (!editing) {
            this.setState({
                editing: {
                    id: id++,
                    coordinates: [e.nativeEvent.coordinate],
                    holes: [],
                },
            });
        } else if (!creatingHole) {
            this.setState({
                editing: {
                    ...editing,
                    coordinates: [
                        ...editing.coordinates,
                        e.nativeEvent.coordinate,
                    ],
                },
            });
        } else {
            const holes = [...editing.holes];
            holes[holes.length - 1] = [
                ...holes[holes.length - 1],
                e.nativeEvent.coordinate,
            ];
            this.setState({
                editing: {
                    ...editing,
                    id: id++, // keep incrementing id to trigger display refresh
                    coordinates: [
                        ...editing.coordinates,
                    ],
                    holes,
                },
            });
        }
    }

    onPress(e, canCreatePolygon, canCreateLine) {
        if(canCreatePolygon){
            this.creatingPolygon(e);
        }
        else if(canCreateLine){
            this.createLine(e);
        }
    }

    createLine(e) {
        editing = this.state.editing;
        if(!editing){
            this.setState({
                editing: {
                    id: id++,
                    coordinates: [e.nativeEvent.coordinate],
                    color: "#AA3939"
                },
            });
        }else{
            this.setState({
                editing: {
                    ...editing,
                    coordinates: [
                        ...editing.coordinates,
                        e.nativeEvent.coordinate,
                    ],
                },
            });
        }
    }

    createElementLayer(index, layerInfo){
        var name = layerInfo.name;
        var isCheck = undefined;
        if(layerInfo.visible){
            isCheck = index;
        }
        return (
            {
                item: {value: index, label: "\n"+name},
                check: isCheck
            }
        )
    }

    // cambia las capas que se muestran
    // Se llama cada vez que hay un cambio en los checkbox
    changeLayers(values){
        this.state.layers.forEach( (layer, index) => {
            find = values.find((v) => {
                return v === index;
            });
            if(find === undefined){
                this.state.layers[index].visible = false;
            }else{
                this.state.layers[index].visible = true;
            }
        });

    }


    crearModal() {
        var items = [];
        var checked = [];
        for(var i = 0; i < this.state.layers.length; i++){
            result = this.createElementLayer(i, this.state.layers[i]);
            items.push(result.item);
            if(result.check !== undefined){
                checked.push(result.check);
             }

        }
        modal = (
            <Modal
                visible={this.state.modalVisible}
                onRequestClose={() => this.setModalVisible(false)}
                animationType={"slide"}
                transparent={true}
                >
                <View style={styles.modalContainer} >
                    <Subheader text="Capas" />
                    <View style={styles.modalContent}>
                        <CheckboxGroup
                            onSelect={(values) => this.changeLayers(values)}
                            checked={checked}
                            items={items}
                        />
                        <Button
                            onPress={this.setModalVisible.bind(this, false)}
                            style={styles.modalButton}
                            text={"Aceptar"}
                            >
                        </Button>
                    </View>
                </View>
            </Modal>
        )
        return modal;
    }

    render() {
        console.log(">>> Render");
        const mapOptions = {
            scrollEnabled: true,
        };


        var routes = this.props.routes;
        var areas = this.props.areas;
        var layers = [];
        this.state.layers.forEach((layer) => {
            if(layer.visible){
                layer.polygons.forEach((pol) => {
                    layers.push(pol);
                })
            } 
        })

        if (this.state.editing) {
            mapOptions.scrollEnabled = false;
            mapOptions.onPanDrag = e => this.onPress(e, this.props.canCreatePolygon, this.props.canCreateLine);
        }
        if(routes === undefined){
            routes = [];
        }
        if(areas === undefined){
            areas = [];
        }

        var modal = undefined;
        var buttonLayer = undefined;
        // crear el modal con las capas disponibles
        if(this.state.layers !== undefined && this.state.layers.length > 0){
            modal = this.crearModal();
            // agrega el fondo oscuro al mostrar el modal
            if(this.state.modalVisible){
                modal = (
                    <View style={styles.fondoModal}
                    >
                        {modal}
                    </View>
                )
            }
            // agregar el boton de las capas
            buttonLayer = (
                <ActionButton position="left" buttonColor="rgba(231,76,60,1)"
                    icon={<Icon name="layers" style={styles.actionButtonIcon} />}
                    onPress={() => this.setModalVisible(true)}
                >
                </ActionButton>
            )
        }



        return (
            <View style ={styles.container}>
                {this.props.toolbar}
                <MapView
                    style={styles.map}
                    initialRegion={{
                        latitude: this.state.lat,
                            longitude: this.state.lng,
                            latitudeDelta: 0.015,
                            longitudeDelta: 0.0121,
                    }}
                    mapType={"satellite"}
                    onPress={e => this.onPress(e, this.props.canCreatePolygon, this.props.canCreateLine)}
                    showsUserLocation={true}
                    showsMyLocationButton={true}
                    showsCompass={true}
                    {...mapOptions}
                >
                    {this.state.polygons.map(polygon => (
                        <MapView.Polygon
                            key={polygon.id}
                            coordinates={polygon.coordinates}
                            holes={polygon.holes}
                            strokeColor="#F00"
                            fillColor="rgba(255,0,0,0.5)"
                            strokeWidth={STROKE_WIDTH}
                        />
                    ))}
                    {(this.state.editing && this.props.canCreatePolygon) && (
                        <MapView.Polygon
                            key={this.state.editing.id}
                            coordinates={this.state.editing.coordinates}
                            holes={this.state.editing.holes}
                            strokeColor="#000"
                            fillColor="rgba(255,0,0,0.5)"
                            tappable={false}
                            strokeWidth={STROKE_WIDTH}
                        />
                    )}

                    {areas.map(area => (
                        <MapView.Polygon
                            key={area.id}
                            coordinates={area.coordinates}
                            strokeColor={area.color}
                            fillColor={area.color}
                            holes={area.holes}
                            strokeWidth={STROKE_WIDTH}
                        />
                    ))}

                    {this.state.editing && this.props.canCreateLine && (
                        <MapView.Polyline
                            key={this.state.editing.id}
                            coordinates={this.state.editing.coordinates}
                            strokeColor={this.state.editing.color}
                            strokeWidth={STROKE_WIDTH}
                        />
                    )}


                    {routes.map(route => (
                        <MapView.Polyline
                          key={route.id}
                          coordinates={route.coordinates}
                          strokeColor={route.color}
                          strokeWidth={STROKE_WIDTH}
                        />
                    ))}

                    {this.state.lines.map(line => (
                        <MapView.Polyline
                          key={line.id}
                          coordinates={line.coordinates}
                          strokeColor={line.color}
                          strokeWidth={STROKE_WIDTH}
                        />
                    ))}

                    {layers && layers.map(layer => (
                        <MapView.Polygon
                            key={layer.id}
                            coordinates={layer.coordinates}
                            strokeColor={"#000"}
                            fillColor={layer.color}
                            holes={layer.holes}
                            strokeWidth={STROKE_WIDTH}
                        />
                    ))}
                    
                    {this.props.personas && this.props.personas.map(p => (
                        <MapView.Marker
                            key={p.key}
                            coordinate={p.pos}
                          >
                        </MapView.Marker>
                      )
                    )}

                </MapView>

                <View style={styles.buttonContainer}>
                    {this.state.editing && (
                        <TouchableOpacity
                        onPress={() => this.createHole()}
                        style={[styles.bubble, styles.button]}
                        >
                        <Text>{this.state.creatingHole ? 'Finish Hole' : 'Create Hole'}</Text>
                        </TouchableOpacity>
                    )}
                    {this.state.editing && (
                        <TouchableOpacity
                        onPress={() => {
                            this.finish(this.props.canCreatePolygon, this.props.canCreateLine);
                            this.props.onFinishPolygon();
                        }}
                        style={[styles.bubble, styles.button]}
                        >
                        <Text>Finish</Text>
                        </TouchableOpacity>
                    )}
                </View>

                {buttonLayer}
                {this.props.children}
                {modal}
            </View>
        );
    }

    setModalVisible(visible){
         this.setState({modalVisible: visible});
    }

}

const styles = StyleSheet.create({
    container: {
        ...StyleSheet.absoluteFillObject,
    },
    map:{
        ...StyleSheet.absoluteFillObject,
        zIndex: 0,
    },
    buttonContainer: {
        flexDirection: 'row',
        marginVertical: 20,
        backgroundColor: 'transparent',
    },
    button: {
        width: 80,
        paddingHorizontal: 12,
        alignItems: 'center',
        marginHorizontal: 10,
    },
    bubble: {
        backgroundColor: 'rgba(255,255,255,0.7)',
        paddingHorizontal: 18,
        paddingVertical: 12,
        borderRadius: 20,
    },
    actionButtonIcon: {
        fontSize: 20,
        height: 22,
        color: 'white',
    },
    modalContainer: {
        backgroundColor: 'rgba(255,255,255,1)',
        //marginTop: 20,
        //marginBottom: 20,
        //marginLeft: 20,
        //marginRight: 20,
        position: 'relative',
        width: 200,
        left: 0,
        right: 0,
        top: 0,
        bottom: 0,
        marginLeft: 'auto',
        marginRight: 'auto',
        marginTop: 'auto',
        marginBottom: 'auto',

        //flex: 1,
        //flexDirection: 'column',
        //justifyContent: 'center',
        //alignItems: 'center',
    },
    modalContent: {
        //flex: 1,
        //flexDirection: 'column',
        //justifyContent: 'center',
        //alignItems: 'center',
        //marginBottom: 10,
        //marginTop: 10,
    },
    layerSwitch: {
        flex: 1,
        flexDirection: 'row',
        justifyContent: 'center',
        alignItems: 'center',
        //width: 100,
    },
    modalButton: {
        marginBottom: 10,
        flex: 1,
        justifyContent: 'flex-end',
    },
    fondoModal: {
        backgroundColor: 'rgba(50,50,50,0.5)', 
        ...StyleSheet.absoluteFillObject,
    },
});

AppRegistry.registerComponent('MapComponent', () => MapComponent);
