

import React, { Component } from 'react';
import {
    AppRegistry,
    View,
    Text
} from 'react-native';



export default class ChatScreen extends Component {
    static navigationOptions = {
        title: "chat"
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
