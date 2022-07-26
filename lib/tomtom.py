#!/usr/bin/env python3
import os
import sys
import argparse
import json
import time
from collections import defaultdict

import git
import geopy.distance
import pandas as pd
import numpy as np

ROOT = git.Repo('.', search_parent_directories=True).working_tree_dir

def parse_road_segments(road):
    shapes = road['shape']
    n_points = len(shapes)
    data = []
    if n_points > 1:
        road_segments = []
        start = (shapes[0]['latitude'], shapes[0]['longitude'])        
        for p in shapes[1:]:
            end = (p['latitude'], p['longitude'])
            dist = geopy.distance.geodesic(start, end).m
            data.append(
                [road['segmentId'], start[0], start[1], end[0], end[1], dist]
            )
            start = end
    colnames = ['id', 'lat1', 'lon1', 'lat2', 'lon2', 'dist']
    return(pd.DataFrame(data = data, columns = colnames))

def parse_speeding(road):
    road_id = road['segmentId']
    road_seg_time_results = road['segmentTimeResults'][0]
    speed_limit = road['speedLimit'] 
    tomtom_exposure = road_seg_time_results['sampleSize']
    percentiles = road_seg_time_results['speedPercentiles']
    hist = np.histogram(
        a = [p - speed_limit for p in percentiles], 
        bins = np.r_[-np.inf, range(-10, 35, 5), np.inf], 
        density=False
    )
    row_vals = (list(hist[0]/len(percentiles)) 
                + [road_id, speed_limit, tomtom_exposure])
    colnames = [
        'below_neg_10','neg_10_neg_5', 'neg_5_to_0', 
        'pos_0_to_5', 'pos_5_to_10', 'pos_10_to_15', 
        'pos_15_to_20', 'pos_20_to_25', 'pos_25_to_30',
        'above_pos_30', 'id', 'speed_limit', 'tomtom_exposure'
    ]
    return(pd.DataFrame(data = [row_vals], columns = colnames))
    
def parse_tomtom(city):
    data_file = os.path.join(ROOT, 'data', city, 'json', f'{ city }.json')
    with open(data_file) as json_file:
        data = json.load(json_file)
    segment_rows = [parse_road_segments(road) for road 
                    in data['network']['segmentResults']]
    speed_rows = [parse_speeding(road) for road 
                  in data['network']['segmentResults']]
    segment_df = pd.concat(segment_rows)
    segment_df.to_csv(
        os.path.join(ROOT, 'data', city, 'segs', f'{ city }_segs.csv'), 
        index = False
    )
    speed_df = pd.concat(speed_rows)
    speed_df.to_csv(
        os.path.join(ROOT, 'data', city, 'speeds', f'{ city }_speed.csv'),
        index = False
    )

def parse_args(argv):
    desc = ('Parses json files from Tomtom into road segments '
            'and speeding distributions.'
           )
    parser = argparse.ArgumentParser(prog=argv[0], description=desc)
    parser.add_argument('cities', nargs='+')
    return parser.parse_args(argv[1:])

if __name__ == '__main__':
    args = parse_args(sys.argv)
    cities = args.cities
    for city in cities:
        parse_tomtom(city)
