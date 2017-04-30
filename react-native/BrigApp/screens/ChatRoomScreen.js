


import React, { Component } from 'react';
import {
    AppRegistry,
    Text,
    View,
    ToolbarAndroid,
    StyleSheet,
} from 'react-native';



export default class ChatRoomScreen extends Component {
    static navigationOptions = {
        title: "chat room",
        header: null,
    }
    constructor(props){
        super(props);
    }

    render(){
        return (
            <View>
                <ToolbarAndroid
                    title="AwesomeApp"
                    style={styles.toolbar}
                />
                <Text>
                    "mapamapa"
                </Text>
            </View>
        )
    }
}

 var styles = StyleSheet.create({ toolbar: { backgroundColor: '#e9eaed', height: 56, }, });
