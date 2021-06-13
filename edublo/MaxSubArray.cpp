#include <iostream>
#include <limits.h>
#include <tuple>
#include <vector>
using namespace std;


tuple<int,int,int> FindMaxCrossingSubArray(vector<int> array, int low, int mid, int high){

    int leftSum = INT_MIN, sum = 0;
    
    int maxLeft = 0;

    for(auto i = mid ; i >= low ; i--){
        sum += array.at(i); 
        if(sum > leftSum){
            leftSum = sum;
            maxLeft = i;
        }
    }

    sum = 0 ;

    int rightSum = INT_MIN;

    int maxRight = 0;

    for(auto i = mid+1 ; i <= high ; i++){
        sum += array.at(i);
        if (sum > rightSum){
            rightSum = sum;
            maxRight = i;
        }
    }

    return tuple<int,int,int>(maxLeft,maxRight, leftSum+rightSum);


}

tuple<int,int,int> FindMaximumSubArray(vector<int> array, int low, int high){

    if( high == low)
        return tuple<int,int,int>(low,high,array.at(low));
    else{
    
        int mid = (high+low)/2;//low + (high-low)/2;

        tuple<int,int,int> First = FindMaximumSubArray(array,low,mid);

        tuple<int,int,int> Second = FindMaximumSubArray(array,mid+1,high);

        tuple<int,int,int>  Third = FindMaxCrossingSubArray(array,low,mid,high);

        if(get<2>(First) >= get<2>(Second) and get<2>(First) >= get<2>(Third))
            return First;
        else if(get<2>(Second) >= get<2>(First) and get<2>(Second) >= get<2>(Third))
            return Second;
        else 
            return Third;

    }

}


int main(){

    vector<int> vec = {13,-3,-25,20,-3,-16,-23,18,20,-7,12,-5,-22,15,-4,7};

    //vector<int> vec = {0,1,2,3,4,5};


    tuple<int,int,int> manin = FindMaximumSubArray(vec,0,vec.size()-1);

    cout<<get<0>(manin)<<'\t'<<get<1>(manin)<<'\t'<<get<2>(manin);



    return 0;
}

