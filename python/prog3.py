

if __name__ == "__main__":
    data = []
    with open("input1.txt", "r") as file:
    	for line in file:
    		data.append(line.split(" "))

    print(data)