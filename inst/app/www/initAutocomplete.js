function initAutocomplete() {

    var searchText = document.getElementById('map-search-address');

    // Limit search to Toronto bounding box - not perfect, but should work?
    var torontoBounds = new google.maps.LatLngBounds(
        new google.maps.LatLng(43.58100, -79.63927),
        new google.maps.LatLng(43.85547, -79.11525));

    var autocomplete = new google.maps.places.Autocomplete(searchText, { bounds: torontoBounds, types: ['geocode'] });
    // Limit to Canada
    autocomplete.setComponentRestrictions({ 'country': ['CA'] });
    autocomplete.setFields(['address_components', 'formatted_address', 'geometry', 'icon', 'name']);
    autocomplete.addListener('place_changed', function () {
        var place = autocomplete.getPlace();
        if (!place.geometry) {
            return;
        }

        var addressPretty = place.formatted_address;
        var address = '';
        if (place.address_components) {
            address = [
                (place.address_components[0] && place.address_components[0].short_name || ''),
                (place.address_components[1] && place.address_components[1].short_name || ''),
                (place.address_components[2] && place.address_components[2].short_name || ''),
                (place.address_components[3] && place.address_components[3].short_name || ''),
                (place.address_components[4] && place.address_components[4].short_name || ''),
                (place.address_components[5] && place.address_components[5].short_name || ''),
                (place.address_components[6] && place.address_components[6].short_name || ''),
                (place.address_components[7] && place.address_components[7].short_name || '')
            ].join(' ');
        }
        var address_number = ''
        address_number = [(place.address_components[0] && place.address_components[0].short_name || '')]
        var coords = place.geometry.location;
        //console.log(address);
        Shiny.onInputChange('map-search-jsValue', address);
        Shiny.onInputChange('map-search-jsValueAddressNumber', address_number);
        Shiny.onInputChange('map-search-jsValuePretty', addressPretty);
        Shiny.onInputChange('map-search-jsValueCoords', coords);
    });
}
