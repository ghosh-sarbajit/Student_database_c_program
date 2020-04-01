//Sarbajit_Ghosh
//CrS1911
#include<stdio.h>
#include<stdlib.h>
#include <string.h>
#define adminpass 7889
#define stupass 1000
typedef struct studenttype{
	int roll;
	char name[20];
	char sub1[20];
	char sub2[20];
	char sub3[20];
	char sub4[20];
	char sub5[20];
	int marks1;
	int marks2;
	int marks3;
	int marks4;
	int marks5;
}stu;


void addrec(FILE *);
void display(FILE *,int);
void display_stu(FILE *,int);//for showing only stu data in stu mode
void delrec(FILE *,FILE *);
void modrec(FILE *,FILE *);


int main()
{
	int class;
	char filename[20];
	int a;
	int pass;
	int ch;
	FILE *fp,*fp1;
	printf("\n\t\tMain Menu\n1.Admin logging\n2.Stu logging\n3.Exit\n");
	scanf("%d",&a);
	if(a==1)
	{
		printf("\nGive admin password:\n");
		scanf("%d",&pass);
		if(pass==adminpass)
		{


		while(1)
		{
		do
		{
		printf("\nEnter which class you want access\n");
		scanf("%d",&class);
		if(class>=1 && class<=12)
		{
			break;
		}
		else
		{
			printf("Please enter a valid class\n");
		}
		}while(1);
		sprintf(filename,"sturec_%d.txt",class);
		printf("\n\tMenu for class %d\n1.Create(For first time)\n2.Display\n3.Add New record\n4.Delete\n5.Update\n6.Exit\n",class);
		printf("Enter your choice\n");
		scanf("%d",&ch);
		switch(ch)
		{
			case(1)://create
			fp=fopen(filename,"w");
			if(fp==NULL){
				printf("Error!! File can not be opened\n");
				break;
			}
			addrec(fp);
			break;

			case(2)://display
			fp=fopen(filename,"r");
			if(fp==NULL){
				printf("Error!! File can not be opened\n");
				break;
			}
			display(fp,class);
			break;

			case(3)://addrec
			fp=fopen(filename,"r+");
			if(fp==NULL){
				printf("Error!! File can not be opened\n");
				break;
			}
			addrec(fp);
			break;

			case(4)://del
			fp=fopen(filename,"r");
			if(fp==NULL){
				printf("Error!! File can not be opened\n");
				break;
			}
			fp1=fopen("temp.txt","w");
			if(fp==NULL){
				printf("Error!! File can not be opened\n");
				break;
			}
			delrec(fp,fp1);
			remove(filename);
			rename("temp.txt",filename);
			break;


			case(5)://modrec
			fp=fopen(filename,"r");
			if(fp==NULL){
				printf("Error!! File can not be opened\n");
				break;
			}
			fp1=fopen("temp.txt","w");
			if(fp==NULL){
				printf("Error!! File can not be opened\n");
				break;
			}
			modrec(fp,fp1);
			remove(filename);
			rename("temp.txt",filename);
			break;


			default:
			exit(0);
		}//end of switch
				
		
	
		}//end of while
		

		}
		else
		{
			printf("Wrong password\n");
			exit(0);
		}
	}
	else if (a==2)
	{
		printf("\nGive student password:\n");
		scanf("%d",&pass);
		if(pass==stupass)
		{
			do
			{
			printf("Enter your class\n");
			scanf("%d",&class);
			if(class>=1 && class<=12)
			{
				break;
			}
			else
			{
				printf("Please enter a valid class\n");
			}
			}while(1);
			sprintf(filename,"sturec_%d.txt",class);
			fp=fopen(filename,"r+");
			if(fp==NULL){
				printf("Error!!\n");
			}
			display_stu(fp,class);
			fclose(fp);
		}
		else
		{
			printf("Wrong password\n");
			exit(0);
		}
	}
	else
	{
		exit(0);
	}
	return 0;
}


void addrec(FILE *fp)
{
	int arr[50],count=0;
	int roll_t,i,found=0;
	stu s1;
	while(fread(&s1,sizeof(stu),1,fp))
	{
		arr[count]=s1.roll;
		count++;
	}
	printf("Enter student record\n");
	while(1)
	{
		printf("Enter roll number :\n");
		scanf("%d",&roll_t);
		for(i=0;i<=count;i++)
		{
			if(roll_t==arr[i])
			{
				found=1;
				break;
			}
		}
		if(found==1)
		{
			found=0;
			printf("Roll exist!!!\n");
			printf("Enter any other roll number\n");
		}
		else
		{
			s1.roll=roll_t;
			break;
		}	
	}
	printf("Enter Name :\n");
	getchar();
	scanf("%[^\n]s",s1.name);
	printf("Enter Subject1 :\n");
	getchar();
	scanf("%[^\n]s",s1.sub1);
	printf("Enter Subject2 :\n");
	getchar();
	scanf("%[^\n]s",s1.sub2);
	printf("Enter Subject3 :\n");
	getchar();
	scanf("%[^\n]s",s1.sub3);
	printf("Enter Subject4 :\n");
	getchar();
	scanf("%[^\n]s",s1.sub4);
	printf("Enter Subject5 :\n");
	getchar();
	scanf("%[^\n]s",s1.sub5);
	
	
	printf("Enter marks of Subject1\n");
	scanf("%d",&s1.marks1);
	printf("Enter marks of Subject2\n");
	scanf("%d",&s1.marks2);
	printf("Enter marks of Subject3\n");
	scanf("%d",&s1.marks3);
	printf("Enter marks of Subject4\n");
	scanf("%d",&s1.marks4);
	printf("Enter marks of Subject5\n");
	scanf("%d",&s1.marks5);
	fwrite(&s1,sizeof(stu),1,fp);
	fclose(fp);
}


void display(FILE *fp,int q)
{
	stu s1;
	printf("==================================================================================================");
	printf("\n\t\t\t\t\tStudent Database Of class %d\n",q);
	printf("--------------------------------------------------------------------------------------------------");
	printf("\nRoll\tName\tSub_1\tSub_2\tSub_3\tSub_4\tSub_5\tMarks_1\tMarks_2\tMarks_3\tMarks_4\tMarks_5\n");
	printf("--------------------------------------------------------------------------------------------------");
	while(1)
		{
			fread(&s1,sizeof(stu),1,fp);
			if(feof(fp)){
				break;
			}
			printf("\n%d\t%s\t%s\t%s\t%s\t%s\t%s\t%d\t%d\t%d\t%d\t%d\n",s1.roll,s1.name,s1.sub1,s1.sub2,s1.sub3,s1.sub4,s1.sub5,s1.marks1,s1.marks2,s1.marks3,s1.marks4,s1.marks5);
		}
	printf("==================================================================================================\n");
		fclose(fp);
}


void delrec(FILE *fp,FILE *fp1)
{
	stu s1;
	int found=0,num;
	printf("\nEnter roll of the record you want delete\n");
		scanf("%d",&num);
		while(1)
		{
			fread(&s1,sizeof(stu),1,fp);
			if(feof(fp)){
				break;
			}
			if(s1.roll!=num)
			{
				fwrite(&s1,sizeof(stu),1,fp1);
			}
			else
			{
				found=1;
			}
		}
		if(found==0)
		{
			printf("Record not found\n");
			fclose(fp);
			fclose(fp1);
		}
		fclose(fp);
		fclose(fp1);
}

void modrec(FILE *fp,FILE *fp1)
{
	stu s1;
	int found=0,num;
	printf("\nEnter roll of the record you want Update\n");
		scanf("%d",&num);
		while(1)
		{
			fread(&s1,sizeof(stu),1,fp);
			if(feof(fp)){
				break;
			}
			if(s1.roll!=num)
			{
				fwrite(&s1,sizeof(stu),1,fp1);
			}
			else
			{
				found=1;
				printf("Enter updated student record\n");
				printf("Enter roll\n");
				scanf("%d",&s1.roll);
				printf("Enter Name :\n");
				getchar();
				scanf("%[^\n]s",s1.name);
				printf("Enter Subject1 :\n");
				getchar();
				scanf("%[^\n]s",s1.sub1);
				printf("Enter Subject2 :\n");
				getchar();
				scanf("%[^\n]s",s1.sub2);
				printf("Enter Subject3 :\n");
				getchar();
				scanf("%[^\n]s",s1.sub3);
				printf("Enter Subject4 :\n");
				getchar();
				scanf("%[^\n]s",s1.sub4);
				printf("Enter Subject5 :\n");
				getchar();
				scanf("%[^\n]s",s1.sub5);
	
	
				printf("Enter marks of Subject1\n");
				scanf("%d",&s1.marks1);
				printf("Enter marks of Subject2\n");
				scanf("%d",&s1.marks2);
				printf("Enter marks of Subject3\n");
				scanf("%d",&s1.marks3);
				printf("Enter marks of Subject4\n");
				scanf("%d",&s1.marks4);
				printf("Enter marks of Subject5\n");
				scanf("%d",&s1.marks5);
				fwrite(&s1,sizeof(stu),1,fp1);
			}
		}
		if(found==0)
		{
			printf("Record not found\n");
			fclose(fp);
			fclose(fp1);
		}
		fclose(fp);
		fclose(fp1);
}

void display_stu(FILE *fp,int q)
{
	stu s1;
	int found=0,num;
	printf("\nEnter roll\n");
		scanf("%d",&num);
		while(fread(&s1,sizeof(stu),1,fp))
		{
			if(s1.roll==num)
			{
				found=1;
				printf("Your record is\n\n");
				printf("==================================================================================================");
				printf("\n\t\t\t\t\tStudent Of class %d\n",q);
				printf("--------------------------------------------------------------------------------------------------");
				printf("\nRoll\tName\tSub_1\tSub_2\tSub_3\tSub_4\tSub_5\tMarks_1\tMarks_2\tMarks_3\tMarks_4\tMarks_5\n");
				printf("--------------------------------------------------------------------------------------------------");
				printf("\n%d\t%s\t%s\t%s\t%s\t%s\t%s\t%d\t%d\t%d\t%d\t%d\n",s1.roll,s1.name,s1.sub1,s1.sub2,s1.sub3,s1.sub4,s1.sub5,s1.marks1,s1.marks2,s1.marks3,s1.marks4,s1.marks5);
				printf("==================================================================================================\n");

			}
		}
		if(found==0)
		{
			printf("Record not found\n");
			fclose(fp);
		}
		fclose(fp);
}
