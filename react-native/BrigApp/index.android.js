
import React, { Component } from 'react';
import {
  AppRegistry,
} from 'react-native';

import {
    StackNavigator,
} from 'react-navigation';

import MapScreen from './screens/MapScreen'
import ChatRoomScreen from './screens/ChatRoomScreen'
import ChatScreen from './screens/ChatScreen'

const BrigApp = StackNavigator({ 
    MapScreen: {screen: MapScreen},
    ChatScreen: {screen: ChatScreen},
    ChatRoomScreen: {screen: ChatRoomScreen},
});


AppRegistry.registerComponent('BrigApp', () => BrigApp);
