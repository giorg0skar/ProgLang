#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdint.h>

#define HALT 0x00
#define JUMP 0x01
#define JNZ 0x02
#define DUP  0x03
#define DROP 0x04
#define PUSH4 0x05
#define PUSH2 0x06
#define PUSH1 0x07
#define ADD 0x08
#define SUB 0x09
#define MUL 0x0A
#define DIV 0x0B
#define MOD 0x0C
#define EQ 0x0D
#define NE 0x0E
#define LT 0x0F
#define GT 0x10
#define LE 0x11
#define GE 0x12
#define NOT 0x13
#define AND 0x14
#define OR 0x15
#define INPUT 0x16
#define OUTPUT 0x17
#define CLOCK 0x2A

typedef struct {
  int32_t *array;
  size_t used;
  size_t size;
} Array;

void initArray(Array *a, size_t initialSize) {
  a->array = (int32_t *) malloc(initialSize * sizeof(int32_t));
  a->used = 0;
  a->size = initialSize;
}

void insertArray(Array *a, int32_t element) {
  // a->used is the number of used entries, because a->array[a->used++] updates a->used only *after* the array has been accessed.
  // Therefore a->used can go up to a->size 
  if (a->used == a->size) {
    a->size *= 2;
    a->array = (int32_t *)realloc(a->array, a->size * sizeof(int32_t));
  }
  a->array[a->used++] = element;
}

void freeArray(Array *a) {
  free(a->array);
  a->array = NULL;
  a->used = a->size = 0;
}


// void push(int l) {
//     sp++;
//     stack[sp]=l;
// }
int counter=0;

int pop(Array *a) {
    if (a->used <= 0) {
        printf("Stack is empty! Cannot pop element\n");
        return -1;
    }
    a->used--;
    return a->array[a->used];
}

int main(int argc, char *argv[]) {
    
    Array *stack;
    int i,dont_care,flag,pc;
    clock_t start,end;
    int lower_bits,upper_bits,addr,res;
    //int res,b1,b2,b3,b4;
    int32_t a,b,b1,b2,b3,b4,top,elem;
    unsigned char *program;    //buffer which contains the entire input text
    char c;
    FILE *fp;

    //printf("The start\n");
    start = clock();
    if (argc != 2) {
        printf("You need to give 1 argument\n");
        return 1;
    }
    fp = fopen(argv[1],"r");
    if (fp==NULL) {
        printf("Error reading input file\n");
        exit(1);
    }
    //program contains the entire input
    program = (unsigned char *) malloc(65536*sizeof(unsigned char));
    //printf("before read\n");
    size_t length = fread(program, 1, 65536, fp);
    
    //printf("before initArray\n");
    stack = (Array *) malloc(sizeof(Array));
    initArray(stack,16);

    //printf("after allocating\n");
    pc =0;
    flag=0;
    //printf("Before while\n");
    while(pc < length) {
        //printf("opcode: %#x\n", program[pc]);
        switch(program[pc]) {
            case HALT:
                flag=1;
                break;
            case JUMP:
                //printf("got in jump\n");
                lower_bits =  program[pc+1];
                upper_bits =  program[pc+2];
                upper_bits = upper_bits << 8;
                addr = upper_bits  | lower_bits;
                if (addr >= length) {
                    printf("Invalid jump address\n");
                    pc+=3;
                    break;
                }
                //pc = &program[0] + addr;
                pc = addr;
                break;
            case JNZ:
                top = pop(stack);
                //printf("element popped was: %d\n",top);
                if ((top==0) || (top == -1)) {
                    pc+=3;
                    break;
                }
                lower_bits = program[pc+1];
                upper_bits = program[pc+2];
                upper_bits = upper_bits << 8;
                addr = upper_bits  | lower_bits;
                if (addr >= length) {
                    printf("Invalid jump address\n");
                    pc+=3;
                    break;
                }
                pc = addr;
                break;
            case DUP:
                i= program[pc+1];
                elem = stack->array[stack->used-i-1];
                //push(stack[elem]);
                insertArray(stack,elem);
                //printf("element pushed to stack: %d\n",elem);
                pc+=2;
                break;
            case DROP:
                dont_care=pop(stack);
                pc++;
                break;
            case PUSH4:
                elem = program[pc+1] | (program[pc+2] << 8) | (program[pc+3] << 16) | (program[pc+4] << 24);
                insertArray(stack,elem);
                //printf("element pushed to stack: %d\n",elem);
                pc+=5;
                break;
            case PUSH2:
                b1 = (int32_t) program[pc+1];
                b2 = ((int32_t) program[pc+2]) << 8;
                elem = b1 | b2;
                insertArray(stack,elem);
                //printf("element pushed to stack: %d\n",elem);
                pc+=3;
                break;
            case PUSH1:
                elem= (int32_t) program[pc+1];
                insertArray(stack,elem);
                //printf("element pushed to stack: %d\n",elem);
                pc+=2;
                break;
            case ADD:
                pc++;
                //we check if the stack has enough elements to perform the operation
                if (stack->used < 2) {
                    printf("Add failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack);
                // if (a == -1) {
                //     printf("Add failed. Not enough elements in stack\n");
                //     break;
                // }
                a = pop(stack);
                // if (b == -1) {
                //     printf("Add failed. Not enough elements in stack\n");
                //     break;
                // }
                insertArray(stack,a+b);
                break;
            case SUB:
                pc++;
                if (stack->used < 2) {
                    printf("Sub failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack);
                a = pop(stack);
                insertArray(stack,a-b);                
                break;
            case MUL:
                pc++;
                if (stack->used < 2) {
                    printf("Mult failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack);
                a = pop(stack);                
                insertArray(stack,a*b);                
                break;
            case DIV:
                pc++;
                if (stack->used < 2) {
                    printf("Div failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack);                
                a = pop(stack);
                if (b != 0) {
                    insertArray(stack,a/b);
                }
                else printf("Attempted to divide by 0. 2 elements of the stack lost\n");                
                break;
            case MOD:
                pc++;
                if (stack->used < 2) {
                    printf("Mod failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack);                
                a = pop(stack);                
                if (b != 0) {
                    insertArray(stack,a % b);
                }
                else printf("Attempted to divide by 0. 2 elements of the stack lost\n");                
                break;
            case EQ:
                pc++;
                if (stack->used < 2) {
                    printf("Equal failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack);
                a = pop(stack);
                res = (a == b);
                insertArray(stack,res);
                break;
            case NE:
                pc++;
                if (stack->used < 2) {
                    printf("Not equal failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack);                
                a = pop(stack);                
                res = a != b;
                insertArray(stack,res);
                break;
            case LT:
                pc++;
                if (stack->used < 2) {
                    printf("Lower than failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack);                
                a = pop(stack);               
                res = a < b;
                insertArray(stack,res);                
                break;
            case GT:
                pc++;
                if (stack->used < 2) {
                    printf("Greater than failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack);                
                a = pop(stack);                
                res = a > b;
                insertArray(stack,res);                
                break;
            case LE:
                pc++;
                if (stack->used < 2) {
                    printf("Lower or equal failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack);                
                a = pop(stack);                
                res = a <= b;
                insertArray(stack,res);                
                break;
            case GE:
                pc++;
                if (stack->used < 2) {
                    printf("Greater or equal failed. Not enough elements in stack\n");
                    break;
                }               
                b = pop(stack);                
                a = pop(stack);                
                res = a >= b;
                insertArray(stack,res);            
                break;
            case NOT:
                pc++;
                if (stack->used < 1) {
                    printf("Not failed. Not enough elements in stack\n");
                    break;
                }
                a = pop(stack);
                insertArray(stack, (a == 0));                
                break;
            case AND:
                pc++;
                if (stack->used < 2) {
                    printf("AND failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack);                
                a = pop(stack);                
                res = a && b;
                insertArray(stack,res);              
                break;
            case OR:
                pc++;
                if (stack->used < 2) {
                    printf("OR failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack);                
                a = pop(stack);                
                res = a || b;
                insertArray(stack,res);               
                break;
            case INPUT:
                scanf("%c",&c);
                insertArray(stack, (int) c);
                pc++;
                break;
            case OUTPUT:
                top = pop(stack);
                if (counter++ == 100) flag=1;
                if (top != -1) printf("%c",top);
                else printf("Stack empty! Cannot pop element.\n");
                pc++;
                break;
            case CLOCK:
                end = clock();
                double time_taken = ((double) (end - start))/CLOCKS_PER_SEC;
                printf("Time passed since the beginning: %0.6lf\n", time_taken);
                pc++;
                break;
            default:
                printf("No instruction matched\n");
                pc++;
        }
        if (flag==1) break;
    }
    //printf("the end\n");

    freeArray(stack);
    fclose(fp);
    return 0;
}
