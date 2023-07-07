

# And what about lateral inflows? Are they mentioned in the network.gr file? 


def parse_line(line) :
    # Input: 100 0 '6' 'Closed end' '' 500 100 '' 'Closed end' '' <
    # Output: ["100", "0", "'6'", "'Closed end'", "''", "500", "100", "''", "'Closed end'", "''", "<"]
    
    in_quote = False
    i_previous = 0
    l = []
    for i in range(len(line)) :
        
        if line[i] == "'" and in_quote :
            in_quote = False
        elif line[i] == "'" and not in_quote :
            in_quote = True
        elif line[i] == " " and in_quote :
            # Keep going
            pass
        elif line[i] == " " and not in_quote :
            # that was a new element
            l.append(line[i_previous:i])
            i_previous = i+1
        else :
            # Normal character
            pass
            
    if i_previous < i :
        # Add last element
        l.append(line[i_previous:i])
    
    return l

def get_locations (path) : 
    # Input: path to a NETWORK.GR file. 
    # Output: a tuple of:
    # - A list of grid points in 5-tuples: (gridpointname, branchname, chainage, x, y)
    # - A list of segments in 5-tuples: (segmentname, branchname, chainage, x, y)
    # Watch out: a segmentname (e.g. 1_4) may be equal to a gridpoint name, but they do not refer to the same concept. 

    # Find grid points
    grid_points = []
    segments = []
    f = open (path)
    
    current_branch_name = None
    previous_segment_name = None
    previous_chainage = None
    previous_x = None
    previous_y = None

    for line in f : 
        # Each line in the file refers to one grid point, and also gives the names of the segments before and after itself. 
        if line.startswith("GRID") : 
            i_ci = line.index("ci")
            i_quote1 = line.index("'", i_ci)
            i_quote2 = line.index("'", i_quote1+1)
            current_branch_name = line[i_quote1+1:i_quote2]
        elif line.strip().endswith(' <') : 
            split_line = parse_line(line)
            chainage = float(split_line[0])
            grid_point_name = split_line[3].replace("'", "")
            segment_name = split_line[4].replace("'", "")
            
            x = 0.0
            y = 0.0
            if len(split_line) >= 7 :
                # x and y not always present in NETWORK.GR
                x = float(split_line[5])
                y = float(split_line[6])
            grid_points.append ((grid_point_name, current_branch_name, chainage, x, y))

            if previous_segment_name != None : 
                avg_chainage = (chainage + previous_chainage) / 2.0
                avg_x = (x + previous_x) / 2.0
                avg_y = (y + previous_y) / 2.0
                segments.append ((previous_segment_name, current_branch_name, avg_chainage, avg_x, avg_y))

            previous_segment_name = segment_name
            previous_chainage = chainage
            previous_x = x
            previous_y = y

    f.close()

    return (grid_points, segments)