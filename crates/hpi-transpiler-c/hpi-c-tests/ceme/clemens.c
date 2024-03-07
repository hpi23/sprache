#include <stdio.h>                                                                                                                                                                                                                        
#include <stdlib.h>                                                                                                                                                                                                                       
#include <stdint.h>                                                                                                                                                                                                                       
#include <string.h>                                                                                                                                                                                                                       
#include <sys/mman.h>                                                                                                                                                                                                                     
                                                                                                                                                                                                                                          
typedef int (*intfun_t)(int);                                                                                                                                                                                                             
                                                                                                                                                                                                                                          
intfun_t repeat(intfun_t fun, int repeats)                                                                                                                                                                                                
{                                                                                                                                                                                                                                         
    int newfun(int arg)                                                                                                                                                                                                                   
    {                                                                                                                                                                                                                                     
        for (int i = 0; i < repeats; i++)                                                                                                                                                                                                 
        {                                                                                                                                                                                                                                 
            arg = fun(arg);                                                                                                                                                                                                               
        }                                                                                                                                                                                                                                 
        return arg;                                                                                                                                                                                                                       
    }                                                                                                                                                                                                                                     
                                                                                                                                                                                                                                          
    int newfun_end(void)                                                                                                                                                                                                                  
    {                                                                                                                                                                                                                                     
        return 0;                                                                                                                                                                                                                         
    }                                                                                                                                                                                                                                     
                                                                                                                                                                                                                                          
    size_t size = (size_t)((intptr_t)newfun_end - (intptr_t)newfun);                                                                                                                                                                      
    char *buffer = (char *)mmap(0, (size_t)size, PROT_EXEC | PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);                                                                                                                 
    memcpy(buffer, (char *)newfun, size);                                                                                                                                                                                                 
                                                                                                                                                                                                                                          
    return (intfun_t)buffer;                                                                                                                                                                                                              
}                                                                                                                                                                                                                                         
                                                                                                                                                                                                                                          
int inc(int arg)                                                                                                                                                                                                                          
{                                                                                                                                                                                                                                         
    return arg + 1;                                                                                                                                                                                                                       
}                                                                                                                                                                                                                                         
                                                                                                                                                                                                                                          
int main(void)                                                                                                                                                                                                                            
{                                                                                                                                                                                                                                         
    intfun_t fp = repeat(inc, 10);                                                                                                                                                                                                        
    int res = fp(5);                                                                                                                                                                                                                      
                                                                                                                                                                                                                                          
    printf("%d\n", res);                                                                                                                                                                                                                  
                                                                                                                                                                                                                                          
    return 0;                                                                                                                                                                                                                             
}  
