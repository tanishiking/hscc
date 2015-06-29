int nums[8];

void bubble(int *data, int num);
void swap(int *data, int a, int b);
int main ();

void swap(int *data, int a, int b){
    int temp;
    temp = data[a];
    data[a] = data[b];
    data[b] = temp;
}

void bubble(int *data, int num){
    int i, j;
    for (i = 0; i < num; i= i+1)
        for(j= num-1; j>i; j=j-1)
            if(data[j-1] > data[j])
                swap(data, j-1, j);
}

int main(){
    int i;
    nums[0] = 5;
    nums[1] = 2;
    nums[2] = 7;
    nums[3] = 10;
    nums[4] = 1;
    nums[5] = 3;
    nums[6] = 4;
    nums[7] = 8;
    bubble(nums, 8);
    for(i = 0; i < 8; i = i + 1) print(nums[i]);
    return 0;
}
