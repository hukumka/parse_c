int main(){
    int** a = field(3, 3);
    return 0;
}

int** field(int h, int w){
    int** res = new int*[h];
    for(int i=0; i<h; ++i){
        res[i] = new int[w];
        for(int j=0; j<w; ++j){
            res[i][j] = i + j;
        }
    }
    
    return res;
}
