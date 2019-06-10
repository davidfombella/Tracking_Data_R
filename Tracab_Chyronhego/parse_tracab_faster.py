def pass_the_tracab(tracab_file, 
                        tracab_meta_file, 
                        remove_officials = True, 
                        add_distance_to_ball = False,
                        add_distance_to_goals = False):
    print("")
    print("~--------------------------------------------~")
    print("|----           Pass the Tracab          ----|")


    ## time estimation
    
    sec_low = 10
    sec_high = 18
    
    if add_distance_to_ball: 
        sec_low = sec_low + 4
        sec_high = sec_high + 6
        
    if add_distance_to_goals: 
        sec_low = sec_low + 4
        sec_high = sec_high + 6

    print("|----   expect between " + str(sec_low) + " - " + str(sec_high) + " seconds   ----|")
    
    ## load metadata 
    meta = parse_tracking_metadata(tracab_meta_file)

    ## open file and store as main block of data 'content'
    with open(tracab_file) as fn:
            content = fn.readlines()

    ## strip the content into lines
    tdat_raw = [x.strip() for x in content]

    ## within each line split into relevant chunks (frame, ball, humans)
    tdat_raw_n = [x.split(":")[0:3] for x in tdat_raw]

    initial_length_of_file_to_parse = len(tdat_raw_n)

    ## calculate the frameIDs
    frameID = [int(f[0]) for f in tdat_raw_n]
    
    ## work out which ones are within the playing time 
    frame_include = [(meta['period1_start'] <= f <= meta['period1_end']) or 
                     (meta['period2_start'] <= f <= meta['period2_end']) or
                     (meta['period3_start'] <= f <= meta['period3_end']) or
                     (meta['period4_start'] <= f <= meta['period4_end']) for f in frameID]

    ## remove frames that are not within the playing time to have a lighter parsing load
    frameID = list(compress(frameID, frame_include))
    tdat_raw_n = list(compress(tdat_raw_n, frame_include))

    # print a report of dropped frames 
    trimmed_length_of_file_to_parse = len(tdat_raw_n)

    print( "|----     " + str(100 - (round(trimmed_length_of_file_to_parse / initial_length_of_file_to_parse, 2) * 100)) + "% of frames discarded      ----|")

    ## Human Segment 

    humans_raw = [f[1].split(";")[:-1] for f in tdat_raw_n]

    frameID_list = []
    team = []
    target_id = []
    jersey_no = []
    x = []
    y = []
    speed = []
    
    for i in range(0,len(humans_raw)):
        
        for p in humans_raw[i]:
            human_parts = p.split(",")
            if human_parts[0] == '1' or human_parts[0] == '0':
                frameID_list.append(float(frameID[i]))
                team.append(float(human_parts[0]))
                target_id.append(float(human_parts[1]))
                jersey_no.append(float(human_parts[2]))
                x.append(float(human_parts[3]))
                y.append(float(human_parts[4]))
                speed.append(float(human_parts[5]))

    tdat = pd.DataFrame(
    {'frameID': frameID_list,
     'team': team,
     'target_id': target_id,
     'jersey_no': jersey_no,
     'x': x,
     'y': y,
     'speed': speed})

    tdat['z'] = 0

    ### BALL SEGMENT 

    ball = [f[2].replace(";","").split(",") for f in tdat_raw_n]
    ball_x = [float(f[0]) for f in ball]
    ball_y = [float(f[1]) for f in ball]
    ball_z = [float(f[2]) for f in ball]
    ball_speed = [float(f[3]) for f in ball]
    ball_owning_team = [f[4] for f in ball]
    ball_status = [f[5] for f in ball]

    balldat = pd.DataFrame(
    {'frameID': frameID,
     'x': ball_x,
     'y': ball_y,
     'z': ball_z,
     'speed': ball_speed,
     'ball_owning_team': ball_owning_team,
     'ball_status': ball_status
    })

    balldat['team'] = 10
    balldat['target_id'] = 100
    balldat['jersey_no'] = 999

    ## MERGE SEGMENT 
    tdat = pd.merge(tdat, balldat[['ball_owning_team', 'ball_status', 'frameID']], on = "frameID" )

    frames = [tdat, balldat]
    tdat = pd.concat(frames, sort=True)

    ### add distance to goals 
    if add_distance_to_goals:
        tdat['dist_goal_1'] = tdat[['x', 'y']].sub(np.array([-5250,0])).pow(2).sum(1).pow(0.5).round()
        tdat['dist_goal_2'] = tdat[['x', 'y']].sub(np.array([5250,0])).pow(2).sum(1).pow(0.5).round()

    ### add distance to ball 
    if add_distance_to_ball:
        
        ball_tdat = balldat[['frameID', 'x', 'y']]
        ball_tdat.columns = ['frameID', 'ball_x', 'ball_y'] # rename to match merge 
        tdat = pd.merge(tdat, ball_tdat, how='left', on=['frameID']) #merge the ball information per frameID
        tdat['dist_2_ball'] = tdat[['x', 'y']].sub(np.array(tdat[['ball_x', 'ball_y']])).pow(2).sum(1).pow(0.5).round()
    
    tdat['match_id'] = re.sub("[^0-9]", "", tracab_file)
    print("|----        MatchID " + re.sub("[^0-9]", "", tracab_file) + " Parsed       ----|")
    
    print("~--------------------------------------------~")
    print("")
    return(tdat)

