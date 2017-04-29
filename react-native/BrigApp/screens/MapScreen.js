
import React, { Component } from 'react';
import {
    AppRegistry,
    View,
    Text
} from 'react-native';



export default class MapScreen extends Component {
    static navigationOptions = {
        headerVisible: false,
    }
    constructor(props){
        super(props);
    }

    render(){
        return (
            <Text>
            "mapamapa"
            </Text>
        )
    }
}
